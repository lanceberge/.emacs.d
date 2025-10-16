;;; -*- lexical-binding: t -*-
(use-package +window
  :ensure nil
  :bind
  (:repeat-map buffer-repeat-map
               ("n" . #'next-buffer)
               ("p" . #'previous-buffer))
  (:map +leader-map
        ("nf" . #'+make-frame)
        ("bd" . #'kill-current-buffer)
        ("bq" . #'+save-and-kill-buffer)
        ("SPC bd" . #'+kill-window-and-buffer)
        ("br" . #'+revert-buffer)
        ("bn" . #'next-buffer)
        ("bp" . #'previous-buffer)))

(use-package window
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map +leader-map
        ("wo" . #'delete-other-windows)
        ("wd" . #'delete-window)
        ("ws" . #'split-window-below)
        ("wv" . #'split-window-right)))

(use-package windmove
  :ensure nil
  :bind
  (:map +leader-map
        ("wj" . #'windmove-down)
        ("wk" . #'windmove-up)
        ("wl" . #'windmove-right)
        ("wh" . #'windmove-left)))

(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?s ?d ?s ?h ?a))
  (aw-scope 'frame)
  :bind
  (:map global-map
        ("M-o" . #'ace-window))
  (:map +leader-map
        ("w SPC" . #'ace-swap-window)))

(use-package winner ; Undo and redo window configs
  :disabled t
  :ensure nil
  :defer 1
  :hook
  (after-init . winner-mode)
  :bind
  (:map +leader-map
        ("wu" . #'winner-undo)
        ("wr" . #'winner-redo)))

(use-package windresize
  :custom
  (windresize-default-increment 3)
  :bind
  (:map +leader-map
        ("wr" . #'windresize))
  (:map windresize-map
        ("h" . #'windresize-left)
        ("l" . #'windresize-right)
        ("k" . #'windresize-up)
        ("j" . #'windresize-down)
        (";" . #'windresize-exit)))

;;;###autoload
(defun +make-frame ()
  (interactive)
  (let ((frame (make-frame)))
    (when (and IS-LINUX (>= emacs-major-version 29))
      (set-frame-parameter frame 'undecorated t))))

;;;###autoload
(defun +revert-buffer ()
  (interactive)
  (revert-buffer t t))
