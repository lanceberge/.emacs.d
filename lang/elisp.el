;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (flycheck-mode -1)))
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer)
  ('emacs-lisp-mode-map
   [remap evil-write] #'+elisp-format-and-check)
  :config
;;;###autoload
  (defun +elisp-format-and-check ()
    "Format buffer, check parens, and save if balanced."
    (interactive)
    (when (eq major-mode 'emacs-lisp-mode)
      (if (balanced-parens-p)
	  (progn
	    (save-buffer))
	(message "Parens are not balanced, saving canceled")))))

(use-package debug
  :ensure nil
  :after evil-collection
  :commands
  (debug-on-entry)
  :config
  (evil-collection-init 'debug))

(use-package edebug
  :ensure nil
  :after evil-collection
  :general
  (my-leader-def
    "ed" #'(edebug-defun :which-key "debug function")
    "e SPC d" #'(edebug-remove-instrumentation :which-key "remove instrumentation"))
  :config
  (evil-collection-init 'edebug))

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  :general
  ('insert 'lispy-mode-map
	   "n" #'special-lispy-down
	   "p" #'special-lispy-up
	   "[" #'self-insert-command
	   "]" #'self-insert-command
	   "\"" #'lispy-doublequote))

(use-package lispyville
  :hook (emacs-lisp-mode . lispyville-mode)
  :general
  ('normal
   [remap evil-])
  :config
  (lispyville-set-key-theme
   '(operators c-w text-objects atom-motions additional-motions
	       commentary slurp/barf-cp)))
