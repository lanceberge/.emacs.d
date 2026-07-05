;;; -*- lexical-binding: t -*-
(use-package window
  :ensure nil
  :bind
  (:map ctl-x-map
        ("6" . #'+window-new-prefix)))

(use-package +other-window-scroll-repeat
  :ensure nil
  :bind
  (:repeat-map +window-scroll-repeat-map
               ("M-v" . #'scroll-other-window-down)
               ("C-v" . #'scroll-other-window)
               ("v" . #'scroll-other-window)
               ("." . #'repeat))
  (:map ctl-x-map
        ("w M-v" . #'scroll-other-window-down)
        ("w C-v" . #'scroll-other-window)
        ("wv" . #'scroll-other-window)))

(use-package window-extras
  :ensure (:type file :main "~/.emacs.d/lisp/window-extras.el" :files ("window-extras.el"))
  :bind
  (:map +leader-map
        ("SPC l" . #'+window-other-buffer))
  :config
  (advice-add 'other-window :before #'+window-split-if-single)
  (setq other-window-scroll-default #'+window-switchy-other-window))

(use-package other-window
  :ensure nil
  :bind
  ("M-o" . #'other-window)
  ("M-O" . #'+window-other-previous)
  :config
  (setq other-window-repeat-map nil))

(use-package ace-window
  :hook
  (emacs-startup . ace-window-display-mode)
  :custom
  (aw-dispatch-when-more-than 1)
  (aw-dispatch-alist
   '((?k aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?j aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?e aw-execute-command-other-window "Execute Command Other Window")
     (?F aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?T aw-transpose-frame "Transpose Frame")
     (?? aw-show-dispatch-help)))
  :bind
  (:map ctl-x-map
        ("wf" . #'ace-window)))


(use-package frame
  :ensure nil
  :bind
  ("s-n" . #'make-frame))

;; switch to the "other window" which is the last window you spent more than `switchy-window-delay' on
(use-package switchy-window
  :hook
  (emacs-startup . switchy-window-minor-mode)
  :bind
  (:map ctl-x-map
        ("wj" . #'switchy-window)))

(use-package windresize
  :custom
  (windresize-default-increment 3)
  :bind
  (:map ctl-x-map
        ("wr" . #'windresize))
  (:map windresize-map
        ("h" . #'windresize-left)
        ("l" . #'windresize-right)
        ("p" . #'windresize-up)
        ("n" . #'windresize-down)
        ("g" . #'windresize-exit)))

(use-package tab-bar-repeat
  :ensure nil
  :bind
  (:repeat-map +tab-bar-repeat-map
               ("]" . #'tab-bar-switch-to-next-tab)
               ("0" . #'tab-bar-close-tab)
               ("[" . #'tab-bar-switch-to-prev-tab))
  (:map +forward-map
        ("t" . #'tab-bar-switch-to-next-tab))
  (:map +backward-map
        ("t" . #'tab-bar-switch-to-prev-tab)))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-name-function #'+window-tab-bar-tab-name-project)
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :bind
  (:map ctl-x-map
        ("pt" . #'+window-other-tab-switch-project)
        ("to" . #'tab-bar-switch-to-recent-tab)
        ("t[" . #'tab-bar-switch-to-prev-tab)
        ("t]" . #'tab-bar-switch-to-next-tab)
        ("tf" . #'+consult-tab)
        ("1" . #'+window-toggle-tab-zoom))
  (:map +leader-map
        ("nt" . #'tab-bar-new-tab)))

(use-package consult-tab-bar
  :ensure (:type file :main "~/.emacs.d/lisp/consult-tab.el" :files ("consult-tab.el"))
  :bind
  (:map ctl-x-map
        ("tf" . #'+consult-tab)))

(use-package tab-bar-repeat
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("[" . #'tab-bar-history-back)
               ("]" . #'tab-bar-history-forward)
               :exit
               ("1" . #'+window-toggle-tab-zoom))
  (:map +backward-map
        ("w" . #'tab-bar-history-back))
  (:map +forward-map
        ("w" . #'tab-bar-history-forward)))

(use-package pulsar
  :hook
  (window-buffer-change-functions . +window-pulsar-pulse-line-no-minibuffer))
