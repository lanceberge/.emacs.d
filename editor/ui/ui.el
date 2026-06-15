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

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

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
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-mode ; higlight hex codes, colors with the color
  :custom (rainbow-x-colors nil) ; don't highlight white, blue, etc.
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package hydra
  :disabled t)

(use-package fringe
  :ensure nil)

(use-package highlight-indent-guides
  :hook
  ((elixir-web-mode
    nxml-mode
    web-mode)
   . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package kanagawa-themes
  :ensure (:host github :repo "lanceberge/kanagawa-emacs"))

(use-package doom-themes)

(use-package theme-rotate
  :ensure (:type file :main "~/.emacs.d/packages/theme-rotate.el")
  :hook (emacs-startup . +theme-rotate-load-current-theme)
  :custom
  (+theme-rotate-dark-themes
   '(gruvbox-dark-hard kanagawa-wave doom-badger doom-oceanic-next
                       doom-tomorrow-night doom-spacegrey doom-tokyo-night
                       doom-palenight doom-Iosvkem doom-one doom-dark+
                       doom-monokai-octagon))
  (+theme-rotate-light-themes
   '(tango kanagawa-paper doom-nord-light doom-oksolar-light
           doom-solarized-light tsdh-light doom-opera-light))
  (+theme-rotate-current-style 'dark)
  :bind
  (:repeat-map +theme-rotate-repeat-map
               ("n" . #'+theme-rotate-rotate)
               ("l" . #'+theme-rotate-toggle-dark-light)
               ("p" . #'+theme-rotate-prev))
  (:map +leader2-map
        ("tn" . #'+theme-rotate-rotate)
        ("tj" . #'+theme-rotate-toggle-dark-light)
        ("tp" . #'+theme-rotate-prev)))

(use-package consult-theme-rotate
  :ensure (:type file :main "~/.emacs.d/packages/consult-theme-rotate.el")
  :after (consult theme-rotate)
  :bind
  (:map +leader2-map
        ("tf" . #'+consult-theme-rotate))
  (:map +consult-theme-rotate-minibuffer-mode-map
        ("M-P" . #'+consult-theme-rotate-toggle-style)))

(use-package theme-rotate-omarchy
  :demand t
  :ensure (:type file :main "~/.emacs.d/packages/theme-rotate-omarchy.el"))
