;;; -*- lexical-binding: t -*-
(use-package frameset
  :ensure nil
  :defer 0.8
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

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

(use-package hl-todo
  :hook ((prog-mode conf-mode yaml-mode yaml-ts-mode yaml-gotmpl-mode) . hl-todo-mode))

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
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-mode ; higlight hex codes, colors with the color
  :custom (rainbow-x-colors nil) ; don't highlight white, blue, etc.
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package fringe
  :ensure nil)

(use-package indent-bars
  :hook
  ((nxml-mode web-mode yaml-mode yaml-ts-mode yaml-gotmpl-mode) . indent-bars-mode))

(use-package indent-bars-extras
  :ensure (:type file :main "~/.emacs.d/lisp/indent-bars-extras.el" :files ("indent-bars-extras.el"))
  :hook
  (elixir-web-mode . +indent-bars-elixir-heex-mode))

(use-package kanagawa-themes
  :ensure (:host github :repo "lanceberge/kanagawa-emacs"))

(use-package doom-themes)
(use-package ef-themes)

(use-package theme-rotate
  :ensure (:type file :main "~/.emacs.d/lisp/theme-rotate.el" :files ("theme-rotate.el"))
  :hook (emacs-startup . +theme-rotate-load-current-theme)
  :custom
  (+theme-rotate-dark-themes
   '(gruvbox-dark-hard kanagawa-wave ef-dream
                       doom-tomorrow-night ef-elea-dark doom-spacegrey
                       doom-dark+))
  (+theme-rotate-light-themes
   '(kanagawa-paper ef-melissa-light ef-eagle ef-orange tango doom-oksolar-light
                    doom-solarized-light doom-opera-light))
  (+theme-rotate-current-style 'dark)
  :bind
  (:repeat-map +theme-rotate-repeat-map
               ("]" . #'+theme-rotate-rotate)
               ("t" . #'+theme-rotate-toggle-dark-light)
               ("[" . #'+theme-rotate-prev)
               ("f" . #'+consult-theme-rotate))
  (:map +forward-map
        ("T" . #'+theme-rotate-rotate))
  (:map +backward-map
        ("T" . #'+theme-rotate-prev))
  (:map +leader-map
        ("tt" . #'+theme-rotate-toggle-dark-light)))

(use-package consult-theme-rotate
  :ensure (:type file :main "~/.emacs.d/lisp/consult-theme-rotate.el" :files ("consult-theme-rotate.el"))
  :after (consult theme-rotate)
  :bind
  (:map +consult-theme-rotate-minibuffer-mode-map
        ("M-P" . #'+consult-theme-rotate-toggle-style)))

(use-package theme-rotate-omarchy
  :defer 5.0
  :ensure (:type file :main "~/.emacs.d/lisp/theme-rotate-omarchy.el" :files ("theme-rotate-omarchy.el")))
