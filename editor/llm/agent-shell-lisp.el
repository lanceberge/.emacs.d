;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +agent-shell-trace--entry-value (entry key)
  "Return ENTRY value for KEY from plist or alist trace entries."
  (if (listp entry)
      (if (keywordp (car entry))
          (plist-get entry key)
        (alist-get key entry))
    nil))

;;;###autoload
(defun +agent-shell-trace--line-position (line)
  "Return point at 1-indexed LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (max 0 (1- line)))
  (point))

;;;###autoload
(defun +agent-shell-trace--visit (entry index total)
  "Visit trace ENTRY and return non-nil when the trace should continue."
  (let* ((file (+agent-shell-trace--entry-value entry :file))
         (line (or (+agent-shell-trace--entry-value entry :line) 1))
         (mark-line (+agent-shell-trace--entry-value entry :mark))
         (description (or (+agent-shell-trace--entry-value entry :description) "Trace location"))
         (expanded-file (and file (expand-file-name file))))
    (unless (and expanded-file (file-readable-p expanded-file))
      (user-error "Trace file is not readable: %s" file))
    (find-file expanded-file)
    (deactivate-mark)
    (let ((start (+agent-shell-trace--line-position line)))
      (goto-char start)
      (when mark-line
        (push-mark (+agent-shell-trace--line-position (1+ mark-line)) t t)
        (goto-char start)
        (activate-mark))
      (recenter))
    (if (= index total)
        (progn
          (message "[%d/%d] %s" index total description)
          nil)
      (y-or-n-p (format "[%d/%d] %s  Next? " index total description)))))

;;;###autoload
(defun +agent-shell-trace (locations)
  "Walk through LOCATIONS by jumping Emacs to each file and line.

LOCATIONS is a list of plist or alist entries.  Each entry accepts
`:file', `:line', optional `:mark', and `:description'.  `:line'
and `:mark' are 1-indexed line numbers; when `:mark' is present,
the region starts at `:line' and ends after `:mark'."
  (interactive "xTrace locations: ")
  (unless (listp locations)
    (user-error "Trace locations must be a list"))
  (let ((total (length locations))
        (index 1)
        (continue t))
    (while (and locations continue)
      (setq continue (+agent-shell-trace--visit (car locations) index total)
            locations (cdr locations)
            index (1+ index)))))
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
