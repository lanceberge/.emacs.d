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
        ("bo" . #'+other-buffer))
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

(use-package pulsar
  :hook
  (window-buffer-change-functions . +window-pulsar-pulse-line-no-minibuffer))
