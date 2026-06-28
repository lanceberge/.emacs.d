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
        ("C-<backspace>" . #'isearch-abort)
        ("C-;" . #'avy-isearch))
  :config
  (setq search-nonincremental-instead nil)) ; don't cancel isearches with searches

(use-package isearch-extensions
  :ensure (:type file :main "~/.emacs.d/lisp/isearch-extensions.el")
  :bind
  (:map isearch-mode-map
        ("M-s l" . #'+consult-line)
        ("M-s L" . #'+consult-line-multi)
        ("M-s ;" . #'+isearch-consult-ripgrep)
        ("M-/" . #'+consult-line)
        ("C-M-/" . #'+consult-line-multi)))
