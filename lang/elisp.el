;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :bind
  (:map +leader-map
        ("es" . #'eval-last-sexp)
        ("ee" . #'eval-expression)
        ("eb" . #'eval-buffer)
        ("ef" . #'eval-defun)))

(use-package +emacs-lisp-extensions
  :ensure (:type file :main "~/.emacs.d/packages/emacs-lisp-extensions.el")
  :hook
  ((emacs-lisp-mode . +elisp--maybe-setup-new-file)
   (emacs-lisp-mode . +elisp-setup-elpaca-rebuild-after-save))
  :bind
  (:map emacs-lisp-mode-map
        ([remap save-buffer] . +elisp-validate-balanced-parens)))

(use-package debug
  :ensure nil
  :commands
  (debug-on-entry))

(use-package edebug
  :ensure nil)
