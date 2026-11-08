;;; Lisp code that i let ai agents evaluate using emacsclient -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'helpful)
(require 'flymake)
(require 'seq)
(require 'subr-x)
(require 'emacs-lisp-extras)

(defvar +agent-lisp-max-lines 2000
  "Maximum number of lines returned by agent Lisp output helpers.")

(defvar +agent-lisp-flymake-timeout 10
  "Seconds to wait for Flymake backends to report diagnostics.")

;;;###autoload
(defun +agent-lisp-eval-buffer (file-name)
  "Evaluate the Emacs Lisp buffer visiting FILE-NAME."
  (with-current-buffer (or (get-file-buffer file-name)
                           (find-file-noselect file-name))
    (eval-buffer)
    (when (fboundp 'elpaca-wait)
      (elpaca-wait))
    "ok"))

;;;###autoload
(defun +agent-lisp-ghostel-update (name status)
  "Update the Ghostel LLM buffer with identity NAME to STATUS. This is run by a claude/codex hook
that uses emacsclient to send command status to emacs."
  (when-let* ((buffer (seq-find
                       (lambda (candidate)
                         (with-current-buffer candidate
                           (and (bound-and-true-p +ghostel-llm-mode)
                                (equal (alist-get 'name ghostel-identity) name))))
                       (buffer-list))))
    (pcase status
      ("running" (+ghostel-llm--set-status buffer "Running"))
      ("ready" (+ghostel-llm--set-status buffer "Ready"))))
  "ok")

;;;###autoload
(defun +agent-lisp-title-from-prompt-async (prompt callback)
  "Generate a concise title from PROMPT, then call CALLBACK with it.
Use a local title when `claude -p' is unavailable or fails."
  (if (not (executable-find "claude"))
      (funcall callback (+agent-lisp--fallback-prompt-title prompt))
    (let* ((output-buffer (generate-new-buffer " *agent-prompt-title*"))
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
       :name "agent-prompt-title"
       :buffer output-buffer
       :command (list shell-file-name "-lc"
                      (format "claude -p %s < /dev/null 2>/dev/null"
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
                      (title (string-trim (substring-no-properties output))))
                 (if (and (= status 0) (not (string-empty-p title)))
                     (funcall callback title)
                   (message "Agent title failed, using fallback: %s"
                            (if (= status 0) "empty title" (string-trim output)))
                   (funcall callback (+agent-lisp--fallback-prompt-title prompt))))
             (when (buffer-live-p (process-buffer process))
               (kill-buffer (process-buffer process))))))))))

;;;###autoload
(defun +agent-lisp--fallback-prompt-title (prompt)
  "Return a local fallback title for PROMPT."
  (let ((words (seq-take (split-string prompt "[^[:alnum:]]+" t) 4)))
    (if words
        (string-join (mapcar #'capitalize words) " ")
      "Untitled Prompt")))

;;;###autoload
(defun +agent-lisp-flymake-diagnostics (file-name)
  "Run Flymake and return diagnostics for FILE-NAME as plists.

Line and column numbers are 1-indexed.  End positions are exclusive."
  (unless (file-exists-p file-name)
    (user-error "File does not exist: %s" file-name))
  (with-current-buffer (or (get-file-buffer file-name)
                           (find-file-noselect file-name))
    (unless flymake-mode
      (user-error "Flymake mode is not enabled in %s" file-name))
    (flymake-start)
    (+agent-lisp--wait-for-flymake)
    (save-restriction
      (widen)
      (mapcar
       (lambda (diagnostic)
         (let ((beg (flymake-diagnostic-beg diagnostic))
               (end (flymake-diagnostic-end diagnostic)))
           (list :file (or buffer-file-name file-name)
                 :line (line-number-at-pos beg t)
                 :column (save-excursion
                           (goto-char beg)
                           (1+ (current-column)))
                 :end-line (line-number-at-pos end t)
                 :end-column (save-excursion
                               (goto-char end)
                               (1+ (current-column)))
                 :type (flymake-diagnostic-type diagnostic)
                 :text (flymake-diagnostic-text diagnostic '(message)))))
       (flymake-diagnostics)))))

;;;###autoload
(defun +agent-lisp--wait-for-flymake ()
  "Wait for the current buffer's running Flymake backends to report."
  (cl-labels
      ((pending-p ()
         (let ((reporting (flymake-reporting-backends)))
           (cl-some (lambda (backend)
                      (not (memq backend reporting)))
                    (flymake-running-backends)))))
    (let ((deadline (+ (float-time) +agent-lisp-flymake-timeout)))
      (while (and (pending-p) (< (float-time) deadline))
        (accept-process-output nil 0.05))
      (when (pending-p)
        (user-error "Timed out waiting for Flymake diagnostics")))))

;;;###autoload
(defun +agent-lisp--truncate-lines (text)
  "Return TEXT truncated to `+agent-lisp-max-lines' lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (forward-line +agent-lisp-max-lines)
    (unless (eobp)
      (delete-region (point) (point-max))
      (insert (format "\n... truncated at %d lines ..." +agent-lisp-max-lines)))
    (buffer-string)))

;;;###autoload
(defun +agent-lisp-function-source (function-name)
  "Return source code for FUNCTION-NAME."
  (let ((symbol (intern-soft function-name)))
    (unless (and symbol (fboundp symbol))
      (user-error "No function named: %s" function-name))
    (pcase-let ((`(,buffer . ,point) (find-function-noselect symbol)))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char point)
            (+agent-lisp--truncate-lines
             (buffer-substring-no-properties
              (point)
              (progn
                (end-of-defun)
                (point))))))))))

;;;###autoload
(defun +agent-lisp-helpful-function (symbol-name)
  "Return Helpful documentation for SYMBOL-NAME without displaying a buffer."
  (let* ((sym (intern-soft symbol-name))
         (callable-p (and sym (fboundp sym)))
         (variable-p (and sym
                          (or (boundp sym)
                              (get sym 'variable-documentation))))
         (text nil))
    (unless (or callable-p variable-p)
      (user-error "No function or variable named: %s" symbol-name))
    (with-temp-buffer
      (helpful-mode)
      (setq helpful--sym sym
            helpful--callable-p callable-p
            helpful--associated-buffer nil
            helpful--first-display t)
      (helpful-update)
      (setq text (buffer-substring-no-properties (point-min) (point-max))))
    (+agent-lisp--truncate-lines text)))

;;;###autoload
(defun +agent-lisp-apropos (pattern)
  "Return Apropos results for PATTERN without displaying a buffer."
  (save-window-excursion
    (let ((display-buffer-overriding-action '((display-buffer-no-window)))
          (help-window-select nil)
          (buffer-name "*Apropos*")
          (text ""))
      (progn
        (apropos pattern)
        (when-let* ((buffer (get-buffer buffer-name)))
          (with-current-buffer buffer
            (setq text (buffer-substring-no-properties
                        (point-min) (point-max))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (+agent-lisp--truncate-lines text))))

;;;###autoload
(defun +agent-lisp-trace (locations)
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
      (setq continue (+agent-lisp-trace--visit (car locations) index total)
            locations (cdr locations)
            index (1+ index)))))

;;;###autoload
(defun +agent-lisp-trace--entry-value (entry key)
  "Return ENTRY value for KEY from plist or alist trace entries."
  (if (listp entry)
      (if (keywordp (car entry))
          (plist-get entry key)
        (alist-get key entry))
    nil))

;;;###autoload
(defun +agent-lisp-trace--line-position (line)
  "Return point at 1-indexed LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (max 0 (1- line)))
  (point))

;;;###autoload
(defun +agent-lisp-trace--visit (entry index total)
  "Visit trace ENTRY and return non-nil when the trace should continue."
  (let* ((file (+agent-lisp-trace--entry-value entry :file))
         (line (or (+agent-lisp-trace--entry-value entry :line) 1))
         (mark-line (+agent-lisp-trace--entry-value entry :mark))
         (description (or (+agent-lisp-trace--entry-value entry :description) "Trace location"))
         (expanded-file (and file (expand-file-name file))))
    (unless (and expanded-file (file-readable-p expanded-file))
      (user-error "Trace file is not readable: %s" file))
    (find-file expanded-file)
    (deactivate-mark)
    (let ((start (+agent-lisp-trace--line-position line)))
      (goto-char start)
      (when mark-line
        (push-mark (+agent-lisp-trace--line-position (1+ mark-line)) t t)
        (goto-char start)
        (activate-mark))
      (recenter))
    (if (= index total)
        (progn
          (message "[%d/%d] %s" index total description)
          nil)
      (y-or-n-p (format "[%d/%d] %s  Next? " index total description)))))

;;;###autoload
(defun +agent-lisp-expand-fisher-stock-template (file-name)
  "Expand the Fisher stock analysis snippet in Org FILE-NAME and save it."
  (let ((file (expand-file-name file-name)))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (unless (derived-mode-p 'org-mode)
        (user-error "Stock analysis template requires an Org file: %s" file))
      (when (save-excursion
              (goto-char (point-min))
              (re-search-forward "^\\* Fisher's 15 points$" nil t))
        (user-error "Fisher stock analysis template already exists in %s" file))
      (+yas-expand-snippet "Fisher 15 stock points"))))

(provide 'agent-lisp)
