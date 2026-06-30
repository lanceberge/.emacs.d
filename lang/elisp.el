;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :bind
  (:map ctl-x-map
        ("e" . #'eval-last-sexp)))

(use-package emacs-lisp-extras
  :ensure (:type file :main "~/.emacs.d/lisp/emacs-lisp-extras.el")
  :hook
  ((emacs-lisp-mode . +elisp--maybe-setup-new-file)
   (emacs-lisp-mode . +emacs-lisp-mode))
  :bind
  (:map emacs-lisp-mode-map
        ([remap save-buffer] . +elisp-validate-balanced-parens)))

(use-package debug
  :ensure nil
  :commands
  (debug-on-entry))

(use-package edebug
  :ensure nil)
