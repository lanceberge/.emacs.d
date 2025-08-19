;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer)
  :general
  ('emacs-lisp-mode-map
   [remap save-buffer] #'+elisp-format-and-check))

(use-package debug
  :ensure nil
  :commands
  (debug-on-entry))

(use-package edebug
  :ensure nil
  :general
  ;; TODO
  ;; (my-leader-def
  ;;   :keymaps 'emacs-lisp-mode-map
  ;;   "ed" #'(edebug-defun :which-key "debug function")
  ;;   "e SPC d" #'(edebug-remove-instrumentation :which-key "remove instrumentation"))
  )

;;;###autoload
(defun +elisp-format-and-check ()
  "Format buffer, check parens, and save if balanced."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (balanced-parens-p)
        (progn
          (save-buffer))
      (message "Parens are not balanced, saving canceled"))))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))
