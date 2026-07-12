;;; -*- lexical-binding: t -*-
(use-package isearch
  :ensure nil
  :hook (isearch-mode-end . +isearch-exit-at-start)
  :custom
  (isearch-lazy-count t)
  (search-invisible nil)
  :bind
  (:map isearch-mode-map
        ("M-P" . #'isearch-toggle-regexp)
        ("C-g" . #'isearch-cancel)
        ("C-<backspace>" . #'isearch-abort))
  (:map search-map
        (".")
        (".s" . #'isearch-forward-symbol-at-point))
  :config
  (setq search-nonincremental-instead nil)) ; don't cancel isearches with searches

(use-package isearch-extras
  :ensure (:type file :main "~/.emacs.d/lisp/isearch-extras.el" :files ("isearch-extras.el"))
  :bind
  (:map isearch-mode-map
        ("C-<return>" . #'+isearch-exit-at-end)
        ("M-s g" . #'+isearch-consult-ripgrep)))
