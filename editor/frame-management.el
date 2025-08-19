;;; -*- lexical-binding: t -*-
(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?s ?d ?s ?h ?a))
  (aw-scope 'frame)
  :general
  ('(global-map)
   "M-o" #'ace-window))

(use-package winner ; Undo and redo window configs
  :ensure nil
  :defer 1
  :hook
  (after-init . winner-mode)
  :general
  (my-leader-def
    "wu" #'(winner-undo :which-key "undo window operation")
    "wr" #'(winner-redo :which-key "redo window operation")))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :general
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
    "po" #'(+open-project :which-key "open project")
    "tr" #'(tab-bar-rename-tab :which-key "rename tab")))

;;;###autoload
(defun +tab-bar/open-and-rename ()
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'tab-bar-rename-tab))

(use-package windresize
  :custom
  (windresize-default-increment 3)
  :general
  (my-leader-def
    "wr" #'windresize)
  ('windresize-map
   "h" #'windresize-left
   "l" #'windresize-right
   "k" #'windresize-up
   "j" #'windresize-down
   ";" #'windresize-exit))
