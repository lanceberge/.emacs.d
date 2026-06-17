;;; Lisp code that i let ai agents evaluate using emacsclient -*- lexical-binding: t -*-

(defvar +agent-lisp-max-lines 2000
  "Maximum number of lines returned by agent Lisp output helpers.")

;;;###autoload
(defun +agent-lisp-eval-buffer (file-name)
  "Revert and evaluate the Emacs Lisp buffer visiting FILE-NAME."
  (with-current-buffer (or (get-file-buffer file-name)
                           (find-file-noselect file-name))
    (revert-buffer t t t)
    (eval-buffer)
    "ok"))

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
        (when-let ((buffer (get-buffer buffer-name)))
          (with-current-buffer buffer
            (setq text (buffer-substring-no-properties
                        (point-min) (point-max))))))
      (when-let ((buffer (get-buffer buffer-name)))
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

(provide 'agent-lisp)
