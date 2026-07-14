;;; -*- lexical-binding: t -*-
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :bind
  (:map ctl-x-map
        ("1" . #'+window-toggle-tab-zoom)))

(use-package tab-bar-history
  :ensure nil
  :bind
  (:repeat-map tab-bar-history-repeat
               ("." . #'repeat)
               ("[" . #'tab-bar-history-back)
               ("]" . #'tab-bar-history-forward)
               :exit
               ("1" . #'delete-other-windows))
  (:map +forward-map
        ("w" . #'tab-bar-history-forward))
  (:map +backward-map
        ("w" . #'tab-bar-history-back)))

(use-package project-tab
  :ensure (:type file :main "~/.emacs.d/lisp/project-tab/project-tab.el" :files ("project-tab.el"))
  :custom
  (tab-bar-tab-name-function #'+project-tab-name-tab-function)
  (+project-tab-other-project-missing-command #'+project-tab-new-project-command)
  :bind
  (:map project-prefix-map
        ("[" . #'+project-tab-prev-project-command)
        ("]" . #'+project-tab-next-project-command)
        ("n" . #'+project-tab-new-project-command)
        ("j" . #'+project-tab-switch-other-project-command)
        ("p" . #'+project-tab-switch-project-command))
  (:map ctl-x-map
        ("tn" . #'+project-tab-new-project-command)
        ("to" . #'+project-tab-other-project-command))
  (:map +leader-map
        ("k" . #'+project-tab-switch-project-command)
        ("j" . #'+project-tab-switch-other-project-command))
  (:map +forward-map
        ("t" . #'+project-tab-next))
  (:map +backward-map
        ("t" . #'+project-tab-prev)))

(use-package consult-project-tab
  :ensure (:type file :main "~/.emacs.d/lisp/project-tab/consult-project-tab.el" :files ("consult-project-tab.el"))
  :bind
  (:map ctl-x-map
        ("tF" . #'+consult-tab)
        ("tf" . #'+consult-project-tab-find)))

(use-package tabspaces
  :demand t
  :custom
  (tabspaces-session-auto-restore t)
  :hook
  (tab-bar-mode . tabspaces-mode))

(use-package tabspace-extras
  :ensure (:type file :main "~/.emacs.d/lisp/tabspace-extras.el" :files ("tabspace-extras.el"))
  :demand t
  :bind
  (:map ctl-x-map
        ("K" . #'kill-current-buffer)
        ("k" . #'+tabspace-kill-buffer-dwim))
  (:map +leader-map
        ("l" . #'+tabspace-other-buffer-dwim)
        ("bo" . #'+tabspace-other-special-buffer-dwim)))
