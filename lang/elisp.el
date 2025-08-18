;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer)
  ('emacs-lisp-mode-map
   [remap evil-write] #'+elisp-format-and-check))

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

(use-package lispyville
  :hook (emacs-lisp-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators c-w commentary slurp/barf-cp))
  :general
  ('meow-normal-state-keymap
   "Y" #'lispyville-yank-line))

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
