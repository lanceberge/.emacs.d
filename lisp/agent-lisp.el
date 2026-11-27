;;; Lisp code that i let ai agents evaluate using emacsclient -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'helpful)
(require 'flymake)
(require 'json)
(require 'lisp-mode)
(require 'seq)
(require 'subr-x)
(require 'emacs-lisp-extras)
(require 'elpaca)

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
  "Update the Ghostel LLM buffer with identity NAME to STATUS.
Claude and Codex hooks call this via emacsclient to report command status."
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
Skip the Checkdoc backend for personal configuration files.
Line and column numbers are 1-indexed.  End positions are exclusive."
  (unless (file-exists-p file-name)
    (user-error "File does not exist: %s" file-name))
  (with-current-buffer (or (get-file-buffer file-name)
                           (find-file-noselect file-name))
    (unless flymake-mode
      (user-error "Flymake mode is not enabled in %s" file-name))
    (let ((flymake-diagnostic-functions (+agent-lisp--flymake-backends)))
      (flymake-start)
      (+agent-lisp--wait-for-flymake))
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
(defun +agent-lisp--flymake-backends ()
  (if (+elisp--user-emacs-file-p)
      (let (backends)
        ;; Collect the effective hook, including inherited backends at `t'.
        (run-hook-wrapped 'flymake-diagnostic-functions
                          (lambda (backend) (push backend backends) nil))
        (remq #'elisp-flymake-checkdoc (nreverse backends)))
    flymake-diagnostic-functions))

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

;;;###autoload
(defun +agent-lisp-read-files (files)
  "Return a JSON array of syntax and autoload records for FILES on disk."
  (decode-coding-string
   (json-serialize (vconcat (mapcar #'+agent-lisp--read-file files))) 'utf-8))

;;;###autoload
(defun +agent-lisp-add-autoloads (file)
  "Insert missing autoload cookies before function definitions in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((original (buffer-string))
           (record (+agent-lisp--scan-lint-source file))
           (positions (alist-get 'missing record)))
      (when (and positions (eq :null (alist-get 'syntax record)))
        (dolist (position (sort positions #'>))
          (goto-char position)
          (skip-chars-backward " \t")
          (unless (bolp) (insert "\n"))
          (insert ";;;###autoload\n"))
        (pcase (+agent-lisp--write-lint-buffer file original)
          ('unsaved (user-error "Save unsaved edits in %s before adding autoloads" file))
          ('stale (user-error "%s changed while adding autoloads; retry" file)))))))

;;;###autoload
(defun +agent-lisp-rebuild-lint-packages (files)
  "Rebuild the local packages provided by FILES and wait for completion."
  (let (packages)
    (dolist (file files)
      (when (+elisp--user-lisp-file-p file)
        (require 'elpaca)
        (with-temp-buffer
          (insert-file-contents file)
          (when-let* ((package (+elisp--provided-package))
                      ((elpaca-get package)))
            (cl-pushnew package packages)))))
    (when packages
      (mapc #'elpaca-rebuild packages)
      (elpaca-wait)
      (dolist (package packages)
        (unless (eq 'finished (elpaca<-status (elpaca-get package)))
          (error "Elpaca rebuild failed for %s" package))))))

;;;###autoload
(defun +agent-lisp-lint-flymake (file)
  "Return Flymake diagnostics for the saved contents of FILE as JSON."
  (let* ((flymake-start-on-flymake-mode nil)
         (enable-local-variables :safe)
         (enable-local-eval nil)
         (visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file t))))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (user-error "Save unsaved edits in %s before running Flymake" file))
      (unless (verify-visited-file-modtime buffer)
        (revert-buffer t t))
      (let ((was-enabled flymake-mode))
        (unwind-protect
            (progn
              (unless was-enabled (flymake-mode 1))
              (let ((diagnostics (+agent-lisp-flymake-diagnostics file)))
                (dolist (diag diagnostics)
                  (plist-put diag :type (symbol-name (plist-get diag :type))))
                (decode-coding-string (json-serialize (vconcat diagnostics)) 'utf-8)))
          (unless was-enabled (flymake-mode -1))
          (unless (or visiting (buffer-modified-p)) (kill-buffer buffer)))))))

;;;###autoload
(defun +agent-lisp--read-file (file)
  "Read syntax and autoload locations from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (assq-delete-all 'missing (+agent-lisp--scan-lint-source file))))

;;;###autoload
(defun +agent-lisp--scan-lint-source (file)
  "Read syntax, function definitions, and local autoloads in the buffer for FILE."
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (let ((symbols-with-pos-enabled t) definitions missing autoloads problem)
    (condition-case err
        (progn
          (forward-comment (point-max))
          (while (not (eobp))
            (let ((form (read-positioning-symbols (current-buffer))))
              (dolist (definition (+agent-lisp--lint-defuns form))
                (save-excursion
                  (goto-char (symbol-with-pos-pos (car definition)))
                  (backward-up-list)
                  (let ((start (point)))
                    (if (and (progn (skip-chars-backward " \t") (bolp))
                             (zerop (forward-line -1))
                             (looking-at-p "^;;;###autoload[ \t]*$"))
                        (push (vector (symbol-name (cadr definition))
                                      (line-number-at-pos)) definitions)
                      (push start missing)))))
              (when (eq (car-safe form) 'use-package)
                (setq autoloads (nconc (+agent-lisp--read-autoloads form) autoloads))))
            (forward-comment (point-max))))
      ((end-of-file invalid-read-syntax)
       (setq problem (vector (line-number-at-pos) (error-message-string err)))))
    `((path . ,file) (definitions . ,(vconcat definitions))
      (autoloads . ,(vconcat autoloads)) (syntax . ,(or problem :null))
      (missing . ,missing))))

;;;###autoload
(defun +agent-lisp--lint-defuns (form)
  "Collect defun and cl-defun forms, skipping quoted data and circular references."
  (let ((pending (list form)) (seen-tails (make-hash-table :test 'eq)) definitions)
    (while pending
      (let ((node (pop pending)))
        (when (and (consp node) (not (gethash node seen-tails)))
          (when (and (memq (car node) '(defun cl-defun))
                     (symbol-with-pos-p (car node))
                     (consp (cdr node)) (symbolp (cadr node))
                     (consp (cddr node)) (listp (nth 2 node)))
            (push node definitions))
          (unless (memq (car node) '(quote \`))
            (let ((args node))
              (while (and (consp args) (not (gethash args seen-tails)))
                (puthash args t seen-tails)
                (when (consp (car args)) (push (car args) pending))
                (setq args (cdr args))))))))
    definitions))

;;;###autoload
(defun +agent-lisp--read-autoloads (form)
  "Return the local recipe and source locations of autoloads in FORM's :init."
  (when (proper-list-p form)
    (let* ((recipe (cadr (memq :ensure (cddr form))))
           (main (and (proper-list-p recipe) (eq (plist-get recipe :type) 'file)
                      (plist-get recipe :main)))
           (init (cdr (memq :init (cddr form))))
           locations)
      (when (stringp main)
        (while (and init (not (keywordp (car init))))
          (pcase (pop init)
            (`(,(and head 'autoload) (,(or 'quote 'function) ,symbol) ,library . ,_)
             (when (and (symbolp symbol) (stringp library))
               (push (vector main (symbol-name symbol) library
                             (line-number-at-pos (symbol-with-pos-pos head)))
                     locations))))))
      locations)))

;;;###autoload
(defun +agent-lisp-apheleia-file (file apply-format)
  "Check FILE with Apheleia and optionally write formatted text.
Return clean, differs, formatted, unsaved, or stale."
  (require 'apheleia)
  (let ((emacs-lisp-mode-hook nil)
        (prog-mode-hook nil))
    (with-temp-buffer
      (setq default-directory (file-name-directory file))
      (insert-file-contents file)
      (emacs-lisp-mode)
      (let ((original (buffer-string)))
        (let ((buffer-file-name file)
              (create-lockfiles nil))
          (+agent-lisp--apheleia-format-buffer))
        (cond
         ((equal original (buffer-string)) 'clean)
         ((not apply-format) 'differs)
         (t (+agent-lisp--write-lint-buffer file original)))))))

;;;###autoload
(defun +agent-lisp--write-lint-buffer (file original)
  "Write the scratch buffer to FILE if disk and visiting buffers permit."
  (let ((visiting (find-buffer-visiting file)))
    (cond
     ((and visiting (buffer-modified-p visiting)) 'unsaved)
     ((not (equal original (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
      'stale)
     (t
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region (point-min) (point-max) file nil 'silent))
      (when visiting
        (with-current-buffer visiting (revert-buffer t t)))
      'formatted))))

;;;###autoload
(defun +agent-lisp--apheleia-format-buffer ()
  "Apply the configured Apheleia formatter to the current scratch buffer."
  (let ((formatters (apheleia--get-formatters))
        (deadline (+ (float-time) 10))
        done failure)
    (unless formatters
      (user-error "Configure an Emacs Lisp formatter in apheleia-mode-alist"))
    (apheleia-format-buffer
     formatters nil :callback (lambda (&rest args)
                                (setq failure (plist-get args :error)
                                      done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless done (error "Apheleia formatting timed out"))
    (when failure (error "Apheleia formatting failed: %s" failure))))

(provide 'agent-lisp)
