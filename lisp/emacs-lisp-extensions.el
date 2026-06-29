;;; -*- lexical-binding: t -*-

;;;###autoload
(define-minor-mode +user-lisp-mode
  "Enable local behavior for Lisp files in `user-emacs-directory'."
  :lighter " User-Lisp"
  (if +user-lisp-mode
      (progn
        (add-hook 'after-save-hook #'+elisp-maybe-elpaca-rebuild nil t)
        (setq-local trusted-content :all)
        (setq-local elisp-flymake-byte-compile-load-path
                    (cons "./" load-path))
        (setq-local flymake-diagnostic-functions
                    '(elisp-flymake-byte-compile))
        (flymake-mode 1))
    (remove-hook 'after-save-hook #'+elisp-maybe-elpaca-rebuild t)
    (flymake-mode -1)
    (kill-local-variable 'trusted-content)
    (kill-local-variable 'elisp-flymake-byte-compile-load-path)
    (kill-local-variable 'flymake-diagnostic-functions)))

;;;###autoload
(defun +emacs-lisp-mode ()
  "Enable local Emacs Lisp behavior for the current buffer."
  (cond
   ((not (+elisp--user-emacs-file-p))
    (flymake-mode 1))
   ((+elisp--user-lisp-file-p)
    (+user-lisp-mode 1))
   (t
    (flymake-mode -1))))

;;;###autoload
(defun +elisp-validate-balanced-parens ()
  "Format buffer, check parens, and save if balanced."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (balanced-parens-p)
        (progn
          (save-buffer))
      (message "Parens are not balanced, saving canceled"))))

;;;###autoload
(defun +elisp-maybe-elpaca-rebuild ()
  (require 'elpaca)
  (when-let* ((file buffer-file-name)
              ((+elisp--user-lisp-file-p file))
              (package (+elisp--provided-package)))
    (run-at-time
     0 nil
     (lambda (package)
       (condition-case err
           (save-window-excursion
             (eval-buffer)
             (message "elpaca rebuilding package %s" package)
             (elpaca-rebuild package)
             (elpaca-process-queues))
         (error
          (message "elpaca-rebuild failed for %S: %s"
                   package (error-message-string err)))))
     package)))

;;;###autoload
(defun +elisp--user-emacs-file-p (&optional file)
  "Return non-nil when FILE or `buffer-file-name' is under `user-emacs-directory'."
  (when-let* ((file (or file buffer-file-name)))
    (file-in-directory-p
     (file-truename file)
     (file-truename user-emacs-directory))))

;;;###autoload
(defun +elisp--user-lisp-file-p (&optional file)
  "Return non-nil when FILE or `buffer-file-name' is under the user lisp directory."
  (when-let* ((file (or file buffer-file-name)))
    (file-in-directory-p
     (file-truename file)
     (file-truename (expand-file-name "lisp/" user-emacs-directory)))))

;;;###autoload
(defun +elisp--provided-package ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (catch 'feature
      (condition-case nil
          (while t
            (when-let* ((form (read (current-buffer)))
                        ((eq (car-safe form) 'provide))
                        (feature-form (cadr form))
                        (feature (if (eq (car-safe feature-form) 'quote)
                                     (cadr feature-form)
                                   feature-form))
                        ((symbolp feature)))
              (throw 'feature feature)))
        (end-of-file nil)
        (invalid-read-syntax nil)))))

;;;###autoload
(defun +elisp--maybe-setup-new-file ()
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
    (if (bound-and-true-p yas-minor-mode)
        (+elisp--setup-new-file)
      (add-hook 'yas-minor-mode-hook #'+elisp--setup-new-file nil t))))

;;;###autoload
(defun +elisp--setup-new-file ()
  "Insert the `lexical-binding' header on new elisp files."
  (require 'yasnippet)
  (+yas-expand-snippet "lexical binding")
  (goto-char (point-max))
  (insert "\n"))

(provide 'emacs-lisp-extensions)
