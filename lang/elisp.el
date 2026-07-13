;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :bind
  (:map ctl-x-map
        ("e" . #'eval-last-sexp))
  (:map +leader-map
        ("td" . #'toggle-debug-on-error)))

(use-package emacs-lisp-extras
  :ensure (:type file :main "~/.emacs.d/lisp/emacs-lisp-extras.el" :files ("emacs-lisp-extras.el"))
  :hook
  ((emacs-lisp-mode . +elisp--maybe-setup-new-file)
   (emacs-lisp-mode . +emacs-lisp-mode))
  :bind
  (:map emacs-lisp-mode-map
        ([remap save-buffer] . +elisp-validate-balanced-parens)))

(use-package elisp-demos ;; help menus for example usages of elisp functions
  :unless IS-WORK
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
