;;; -*- lexical-binding: t -*-
(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?s ?d ?s ?h ?a))
  (aw-scope 'frame)
  :bind
  (:map global-map
        ("M-o" . #'ace-window)))

(use-package winner ; Undo and redo window configs
  :ensure nil
  :defer 1
  :hook
  (after-init . winner-mode)
  :general
  (my-leader-def
    "wu" #'winner-undo
    "wr" #'winner-redo))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :general
  (my-leader-def
    "to" #'+tab-bar/open-and-rename
    "tl" #'tab-bar-switch-to-recent-tab
    "t SPC g" #'tab-bar-select-tab
    "tg" #'tab-bar-switch-to-tab
    "tn" #'tab-bar-switch-to-next-tab
    "tp" #'tab-bar-switch-to-prev-tab
    "t SPC d" #'tab-bar-close-tab-by-name
    "t,d" #'tab-bar-close-other-tabs
    "td" #'tab-bar-close-tab
    "tu" #'tab-bar-undo-close-tab
    "t SPC r" #'tab-bar-rename-tab-by-name
    "po" #'+open-project
    "tr" #'tab-bar-rename-tab))

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
  :bind
  (:map windresize-map
        ("h" . #'windresize-left)
        ("l" . #'windresize-right)
        ("k" . #'windresize-up)
        ("j" . #'windresize-down)
        (";" . #'windresize-exit)))
