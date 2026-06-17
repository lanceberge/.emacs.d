;;; agent-shell-extensions.el --- Extensions for agent-shell -*- lexical-binding: t -*-

(require 'agent-shell)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;###autoload
(defun +agent-shell-toggle-dwim ()
  "Toggle agent shell display, starting one if none exists."
  (interactive)
  (condition-case nil
      (agent-shell-toggle)
    (user-error
     (agent-shell))))

;;;###autoload
(defun +agent-shell-send-region-with-prompt (prompt &optional arg)
  "Send agent-shell DWIM context with PROMPT without focusing the shell.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick
an existing shell."
  (interactive (list (read-string "Agent prompt: ") current-prefix-arg))
  (save-window-excursion
    (unless (region-active-p)
      (beginning-of-line)
      (set-mark (push-mark (line-end-position) nil t)))
    (let (shell-buffer context)
      (cl-letf (((symbol-function 'agent-shell-insert)
                 (lambda (&rest args)
                   (setq shell-buffer (plist-get args :shell-buffer)
                         context (plist-get args :text)))))
        (agent-shell-send-dwim arg))
      (unless shell-buffer
        (setq shell-buffer (agent-shell--shell-buffer :no-create t)))
      (agent-shell-insert
       :text (+agent-shell--format-region-prompt prompt context)
       :submit t
       :no-focus t
       :shell-buffer shell-buffer))))

;;;###autoload
(defun +agent-shell--toggle-shell-buffer ()
  "Return the shell buffer to use for `+agent-shell-toggle'."
  (or (agent-shell--current-shell)
      (seq-first (agent-shell-project-buffers))
      (seq-first (agent-shell-buffers))
      (agent-shell--shell-buffer)))

;;;###autoload
(defun +agent-shell--toggle-viewport (shell-buffer)
  "Toggle the viewport for SHELL-BUFFER."
  (let ((viewport-buffer (agent-shell-viewport--buffer
                          :shell-buffer shell-buffer)))
    (if-let ((window (get-buffer-window viewport-buffer)))
        (quit-restore-window window 'bury)
      (agent-shell-viewport--show-buffer :shell-buffer shell-buffer))))

;;;###autoload
(defun +agent-shell--toggle-shell-window (shell-buffer)
  "Toggle the window for SHELL-BUFFER."
  (if-let ((window (get-buffer-window shell-buffer)))
      (quit-restore-window window 'bury)
    (agent-shell--display-buffer shell-buffer)))

;;;###autoload
(defun +agent-shell--format-region-prompt (prompt context)
  "Return PROMPT combined with CONTEXT."
  (let ((prompt (string-trim prompt)))
    (cond
     ((string-empty-p prompt) context)
     ((string-empty-p (or context "")) prompt)
     (t (concat prompt "\n\n" context)))))

;;;###autoload
(defun +agent-shell-make-permission (rules)
  "Return a responder function built from RULES.

RULES is an alist with two entries:

  (allow (KIND . PATTERNS) ...)
  (ask   (KIND . PATTERNS) ...)

KIND is one of `read', `write', or `execute'.  For `read' and
`write', PATTERNS are path prefixes (expanded via
`expand-file-name').  For `execute', PATTERNS are shell-style
globs where `*' matches any sequence of characters.

The `ask' group takes priority over `allow': a subject matching
both falls back to the interactive prompt.  Subjects matching
neither group also fall back to the prompt.

The returned function is suitable for
`agent-shell-permission-responder-function'."
  (lambda (permission)
    (let* ((tool-call (map-elt permission :tool-call))
           (rule-kind (+agent-shell--permission-rule-kind
                       (map-elt tool-call :kind)))
           (subject (and rule-kind
                         (+agent-shell--permission-subject tool-call rule-kind)))
           (matcher (if (eq rule-kind 'execute)
                        #'+agent-shell--match-command-p
                      #'+agent-shell--match-path-p))
           (ask-patterns (map-elt (cdr (assq 'ask rules)) rule-kind))
           (allow-patterns (map-elt (cdr (assq 'allow rules)) rule-kind)))
      (cond
       ((or (null rule-kind) (null subject)) nil)
       ((funcall matcher subject ask-patterns) nil)
       ((funcall matcher subject allow-patterns)
        (when-let ((option (seq-find
                            (lambda (opt)
                              (equal (map-elt opt :kind) "allow_once"))
                            (map-elt permission :options))))
          (funcall (map-elt permission :respond)
                   (map-elt option :option-id))
          t))))))

;;;###autoload
(defun +agent-shell--permission-subject (tool-call rule-kind)
  "Extract the subject (path or command) from TOOL-CALL for RULE-KIND."
  (pcase rule-kind
    ('execute (or (map-elt tool-call :command)
                  (map-elt tool-call :title)))
    ((or 'read 'write)
     (let ((raw-input (map-elt tool-call :raw-input)))
       (or (and raw-input
                (or (map-elt raw-input 'filepath)
                    (map-elt raw-input 'fileName)
                    (map-elt raw-input 'path)
                    (map-elt raw-input 'file_path)))
           (map-elt tool-call :title))))))

;;;###autoload
(defun +agent-shell--glob-to-regexp (pattern)
  "Convert a shell-style glob PATTERN to an anchored regexp.
Only `*' is treated as a wildcard; all other characters are matched literally."
  (concat "\\`"
          (replace-regexp-in-string
           (regexp-quote "\\*") ".*"
           (regexp-quote pattern))
          "\\'"))

;;;###autoload
(defun +agent-shell--match-path-p (subject patterns)
  "Return non-nil if SUBJECT matches any prefix or glob in PATTERNS.
Both SUBJECT and patterns are expanded with `expand-file-name'.
A pattern containing `*' is matched as an anchored glob where `*'
matches any characters (including `/').  Other patterns are matched
as literal path prefixes."
  (when (stringp subject)
    (let ((expanded (expand-file-name subject)))
      (seq-some (lambda (pattern)
                  (let ((expanded-pattern (expand-file-name pattern)))
                    (if (string-match-p "\\*" expanded-pattern)
                        (string-match-p (+agent-shell--glob-to-regexp expanded-pattern)
                                        expanded)
                      (string-prefix-p expanded-pattern expanded))))
                patterns))))

;;;###autoload
(defun +agent-shell--match-command-p (subject patterns)
  "Return non-nil if SUBJECT (a command string) matches any glob in PATTERNS."
  (when (stringp subject)
    (seq-some (lambda (pattern)
                (string-match-p (+agent-shell--glob-to-regexp pattern)
                                subject))
              patterns)))

;;;###autoload
(defun +agent-shell--permission-rule-kind (acp-kind)
  "Map an ACP tool-call kind string to a rule kind symbol."
  (pcase acp-kind
    ("read" 'read)
    ("write" 'write)
    ("edit" 'write)
    ("execute" 'execute)))

(provide 'agent-shell-extensions)
;;; agent-shell-extensions.el ends here
