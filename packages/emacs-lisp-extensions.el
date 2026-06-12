;;; -*- lexical-binding: t -*-

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
(defun +elisp-setup-elpaca-rebuild-after-save ()
  (add-hook 'after-save-hook #'+elisp-maybe-elpaca-rebuild nil t))

;;;###autoload
(defun +elisp-maybe-elpaca-rebuild ()
  (require 'elpaca)
  (when-let* ((file buffer-file-name)
              ((file-in-directory-p
                (file-truename file)
                (file-truename (expand-file-name "packages/" user-emacs-directory))))
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
(defun +elisp--provided-package ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (catch 'feature
      (let ((read-eval nil))
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
          (invalid-read-syntax nil))))))

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

(provide '+emacs-lisp-extensions)
