;;; -*- lexical-binding: t -*-
(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?s ?d ?s ?h ?a))
  (aw-scope 'frame)
  :general
  ('(normal visual insert)
   "M-o" #'ace-window)

  ('evil-window-map
   "m" #'(ace-swap-window :which-key "move")))

(use-package winner ; Undo and redo window configs
  :ensure nil
  :after evil
  :defer 1
  :hook
  (after-init . winner-mode)
  :general
  ('evil-window-map
   "u" #'(winner-undo :which-key "undo window operation")
   "r" #'(winner-redo :which-key "redo window operation")))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :general
  ('normal
   "]t" #'(tab-bar-switch-to-next-tab :which-key "next tab")
   "[t" #'(tab-bar-switch-to-prev-tab :which-key "next-tab")
   "]T" #'(tab-bar-move-tab :which-key "move tab right")
   "[T" #'(tab-bar-move-tab-to :which-key "move tab left"))

  (my-leader-def
    "to" #'(+tab-bar/open-and-rename :which-key "new tab")
    "tl" #'(tab-bar-switch-to-recent-tab :which-key "last tab")
    "t SPC g" #'(tab-bar-select-tab :which-key "choose tab")
    "tg" #'(tab-bar-switch-to-tab :which-key "choose tab by name")
    "tn" #'(tab-bar-switch-to-next-tab :which-key "next tab")
    "tp" #'(tab-bar-switch-to-prev-tab :which-key "previous tab")
    "t SPC d" #'(tab-bar-close-tab-by-name :which-key "close tab by name")
    "t,d" #'(tab-bar-close-other-tabs :which-key "close other tabs")
    "td" #'(tab-bar-close-tab :which-key "close tab")
    "tu" #'(tab-bar-undo-close-tab :which-key "undo close tab")
    "t SPC r" #'(tab-bar-rename-tab-by-name :which-key "rename tab by name")
    "tr" #'(tab-bar-rename-tab :which-key "rename tab")))

(defun +tab-bar/open-and-rename ()
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'tab-bar-rename-tab))

(use-package windresize
  :after evil
  :custom
  (windresize-default-increment 3)
  :general
  ('evil-window-map
   "SPC r" #'windresize)
  ('windresize-map
   "h" #'windresize-left
   "l" #'windresize-right
   "k" #'windresize-up
   "j" #'windresize-down
   ";" #'windresize-exit))
