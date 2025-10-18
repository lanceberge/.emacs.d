;;; -*- lexical-binding: t -*-
;; don't restore frames with desktop
(use-package frameset
  :ensure nil
  :after desktop
  :demand t
  :config
  (setq frameset-filter-alist
        (append '((background-color . :never)
                  (foreground-color . :never)
                  (background-mode . :never)
                  (cursor-color . :never)
                  (mouse-color . :never)
                  (border-color . :never)
                  (scroll-bar-foreground . :never)
                  (scroll-bar-background . :never))
                frameset-filter-alist)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package white-sand-theme)

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

(use-package gruvbox-theme
  :ensure (:host github :repo "lanceberge/emacs-theme-gruvbox"))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package transient
  :bind
  (:map transient-base-map
        ("M-p" . #'transient-reset))
  :config
  (transient-bind-q-to-quit))

(use-package paren ; show matching parentheses
  :ensure nil
  :hook ((prog-mode text-mode) . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t))

(use-package rainbow-mode ; higlight hex codes, colors with the color
  :custom (rainbow-x-colors nil) ; don't highlight white, blue, etc.
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package hydra
  :disabled t)

(use-package fringe
  :ensure nil)
