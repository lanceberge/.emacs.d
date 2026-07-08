;;; agent-shell-extras.el --- Extensions for agent-shell -*- lexical-binding: t -*-

(require 'agent-shell)
(require 'cl-lib)
(require 'consult-buffer-extras)
(require 'seq)
(require 'subr-x)

(defvar-local +agent-shell--sent-first-prompt nil)

;;;###autoload
(defun +consult-buffer-agent-shell (&optional arg)
  "Switch to an agent shell buffer, or create one with ARG."
  (interactive "P")
  (+consult-buffer--project-dwim "Agent" #'agent-shell-openai-start-codex nil arg))

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
(define-minor-mode +agent-shell-auto-rename-mode
  "Rename the current agent shell buffer from its first submitted prompt."
  :lighter nil
  (setq-local +agent-shell--sent-first-prompt nil)
  (when +agent-shell-auto-rename-mode
    (+agent-shell--ensure-auto-rename-first-submit-advice)))

;;;###autoload
(defun +agent-shell--shell-maker-submit (orig &rest args)
  "Call ORIG with ARGS and auto-rename agent shell buffers when enabled."
  (let ((prompt (+agent-shell--pending-submit-prompt args))
        (should-rename (and +agent-shell-auto-rename-mode
                            (not +agent-shell--sent-first-prompt))))
    (prog1 (apply orig args)
      (when (and should-rename
                 (stringp prompt)
                 (not (string-empty-p prompt)))
        (setq +agent-shell--sent-first-prompt t)
        (+agent-shell--rename-from-first-prompt-async prompt (current-buffer))))))

;;;###autoload
(defun +agent-shell--ensure-auto-rename-first-submit-advice ()
  "Ensure `shell-maker-submit' can dispatch to auto-renaming minor modes."
  (unless (advice-member-p #'+agent-shell--shell-maker-submit
                           'shell-maker-submit)
    (advice-add 'shell-maker-submit
                :around #'+agent-shell--shell-maker-submit)))

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
(defun +agent-shell--pending-submit-prompt (args)
  "Return the prompt about to be submitted from shell-maker ARGS."
  (let ((input (plist-get args :input)))
    (if (stringp input)
        input
      (buffer-substring-no-properties (comint-line-beginning-position)
                                      (point-max)))))

;;;###autoload
(defun +agent-shell--rename-from-first-prompt-async (prompt shell-buffer)
  "Use `claude -p' to title SHELL-BUFFER from PROMPT asynchronously."
  (when (executable-find "claude")
    (let* ((output-buffer (generate-new-buffer " *agent-shell-title*"))
           (instruction
            (concat
             "Create a 1-4 word concise Title Case summary of this prompt to represent the title of this conversation. "
             "Return only the title, with no quotes or punctuation.\n\n"
             prompt))
           (process-environment
            (append (list (concat "ANTHROPIC_API_KEY=" (gptel-api-key))
                          "TERM=dumb"
                          "NO_COLOR=1")
                    process-environment)))
      (make-process
       :name "agent-shell-title"
       :buffer output-buffer
       :command (list shell-file-name "-lc"
                      (format "claude -p %s < /dev/null"
                              (shell-quote-argument instruction)))
       :connection-type 'pipe
       :noquery t
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (unwind-protect
               (let* ((status (process-exit-status process))
                      (output (with-current-buffer (process-buffer process)
                                (buffer-string)))
                      (title (+agent-shell--clean-first-prompt-title output)))
                 (cond
                  ((not (= status 0))
                   (message "agent-shell title failed, using fallback: %s"
                            (string-trim output))
                   (when (buffer-live-p shell-buffer)
                     (+agent-shell--rename-shell-buffer
                      (+agent-shell--fallback-first-prompt-title prompt)
                      shell-buffer)))
                  ((string-empty-p title)
                   (message "agent-shell title failed, using fallback: empty title")
                   (when (buffer-live-p shell-buffer)
                     (+agent-shell--rename-shell-buffer
                      (+agent-shell--fallback-first-prompt-title prompt)
                      shell-buffer)))
                  ((buffer-live-p shell-buffer)
                   (+agent-shell--rename-shell-buffer title shell-buffer))))
             (when (buffer-live-p (process-buffer process))
               (kill-buffer (process-buffer process))))))))))

;;;###autoload
(defun +agent-shell--rename-shell-buffer (title shell-buffer)
  "Rename SHELL-BUFFER using TITLE, preserving an existing viewport name."
  (let* ((old-name (buffer-name shell-buffer))
         (viewport-buffer (+agent-shell--viewport-buffer-for-name old-name)))
    (with-current-buffer shell-buffer
      (shell-maker-set-buffer-name
       shell-buffer
       (generate-new-buffer-name
        (format "%s -- %s" (+agent-shell--buffer-prefix old-name) title))))
    (when (buffer-live-p viewport-buffer)
      (with-current-buffer viewport-buffer
        (rename-buffer
         (generate-new-buffer-name
          (concat (buffer-name shell-buffer) agent-shell-viewport--suffix))
         t)))))

;;;###autoload
(defun +agent-shell--clean-first-prompt-title (title)
  "Clean TITLE returned by `claude -p'."
  (string-trim (substring-no-properties title)))
;; (let ((title (substring-no-properties title)))
;; (setq title (replace-regexp-in-string "\e\\[[?0-9;]*[a-zA-Z]" "" title))
;; (setq title (replace-regexp-in-string "\\[\\?[0-9;]*[a-zA-Z]" "" title))
;; (setq title (string-trim title))
;; (setq title (replace-regexp-in-string "\\`Title:[ \t]*" "" title))
;; (setq title (replace-regexp-in-string "[\n\r\t ]+" " " title))
;; (setq title (replace-regexp-in-string "\\`[\"'`]+\\|[\"'`.!?:;]+\\'" "" title))
;; (truncate-string-to-width (string-trim title) 48 nil nil "...")))

;;;###autoload
(defun +agent-shell--fallback-first-prompt-title (prompt)
  "Return a local fallback title for PROMPT."
  (let* ((words (split-string prompt "[^[:alnum:]]+" t))
         (words (seq-take words 4)))
    (if words
        (string-join (mapcar #'capitalize words) " ")
      "Untitled Prompt")))

;;;###autoload
(defun +agent-shell--buffer-prefix (buffer-name)
  "Return the agent prefix from BUFFER-NAME."
  (replace-regexp-in-string "\\(?: @ .*\\| -- .*\\)\\'" "" buffer-name))

;;;###autoload
(defun +agent-shell--viewport-buffer-for-name (shell-buffer-name)
  "Return the viewport buffer for SHELL-BUFFER-NAME, if one exists."
  (when (boundp 'agent-shell-viewport--suffix)
    (get-buffer (concat shell-buffer-name agent-shell-viewport--suffix))))

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
    ('execute (or (agent-shell--tool-call-command-to-string
                   (map-elt (map-elt tool-call :raw-input) 'command))
                  (map-elt tool-call :command)
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

(provide 'agent-shell-extras)
;;; agent-shell-extras.el ends here
