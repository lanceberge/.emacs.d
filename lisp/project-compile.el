;;; project-compile.el --- Project compile history helpers -*- lexical-binding: t -*-

(require 'compile)
(require 'project)
(require 'savehist)

(defvar +project-compile-histories nil
  "Compile command histories keyed by expanded project root.")

(defvar +project-compile-history nil
  "Dynamically bound compile command history for the current project.")

;;;###autoload
(defun +project-compile-history-get (root)
  "Return the compile command history for project ROOT."
  (alist-get root +project-compile-histories nil nil #'equal))

;;;###autoload
(defun +project-compile-history-put (root history)
  "Store HISTORY as the compile command history for project ROOT."
  (setf (alist-get root +project-compile-histories nil nil #'equal) history))

;;;###autoload
(defun +project-compile (&optional prefix)
  "Run `compile' in the project root with project-local command history."
  (interactive (list current-prefix-arg))
  (let* ((root (expand-file-name (project-root (project-current t))))
         (default-directory root)
         (compilation-buffer-name-function
          (or project-compilation-buffer-name-function
              compilation-buffer-name-function))
         (project-history (copy-sequence
                           (+project-compile-history-get root)))
         (+project-compile-history project-history)
         (default-command (or (car +project-compile-history)
                              (eval compile-command)))
         (command (if (or compilation-read-command prefix)
                      (read-shell-command
                       "Compile command: "
                       nil
                       '+project-compile-history)
                    default-command)))
    (+project-compile-history-put
     root
     (cons command (delete command project-history)))
    (let ((compile-command default-command))
      (compile command (consp prefix)))))

;;;###autoload
(define-minor-mode +project-compile-save-hist-mode
  "Persist project compile command histories with `savehist'."
  :global t
  (if +project-compile-save-hist-mode
      (progn
        (add-to-list 'savehist-additional-variables
                     '+project-compile-histories)
        (add-to-list 'savehist-ignored-variables
                     '+project-compile-history))
    (setq savehist-additional-variables
          (delq '+project-compile-histories savehist-additional-variables))
    (setq savehist-ignored-variables
          (delq '+project-compile-history savehist-ignored-variables))))

(provide 'project-compile)
;;; project-compile.el ends here
