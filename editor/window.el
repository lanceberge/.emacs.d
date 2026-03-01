;;; -*- lexical-binding: t -*-
(setq tab-bar-show nil)

(use-package +window
  :ensure nil
  :bind
  (:repeat-map buffer-repeat-map
               ("n" . #'next-buffer)
               ("p" . #'previous-buffer))
  (:map +leader-map
        ("bd" . #'kill-current-buffer)
        ("bq" . #'+save-and-kill-buffer)
        ("br" . #'+revert-buffer)
        ("SPC l" . #'+other-buffer)
        ("bn" . #'next-buffer)
        ("bp" . #'previous-buffer)))

(use-package other-window
  :ensure nil
  :bind
  ("M-o" . #'other-window)
  ("M-O" . #'other-window-previous))

;;;###autoload
(defun other-window-previous ()
  (interactive)
  (setq repeat-map 'other-window-repeat-map)
  (other-window -1))

(use-package frame
  :ensure nil
  :bind
  (:repeat-map other-frame-repeat-map
               ("n" . #'other-frame)
               ("p" . #'other-frame-previous))
  (:map +leader2-map
        ("fn" . #'other-frame)
        ("fp" . #'other-frame-previous)
        ("f SPC" . #'make-frame)
        ("fg" . #'select-frame-by-name)
        ("fd" . #'delete-frame)))

;;;###autoload
(defun other-frame-previous ()
  (interactive)
  (other-frame -1))

(use-package ace-window
  :custom
  (aw-keys '(?j ?k ?l ?s ?d ?s ?h ?a))
  (aw-scope 'frame)
  :bind
  (:map +leader-map
        ("w SPC" . #'ace-swap-window)))

(use-package window
  :ensure nil
  :custom
  (recenter-positions '(middle top))
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map +leader-map
        ;; ("wo" . #'delete-other-windows)
        ;; ("wd" . #'delete-window)
        ;; ("ws" . #'split-window-below)
        ;; ("wv" . #'split-window-right)
        ))

(use-package windmove
  :ensure nil
  :bind
  (:map +leader-map
        ("wj" . #'windmove-down)
        ("wk" . #'windmove-up)
        ("wl" . #'windmove-right)
        ("wh" . #'windmove-left)))

(use-package winner ; Undo and redo window configs
  :ensure nil
  :hook
  (after-init . winner-mode)
  :bind
  (:repeat-map winner-repeat-map
               ("p" . #'winner-undo)
               ("n" . #'winner-redo))
  (:map +leader-map
        ("wp" . #'winner-undo)
        ("wn" . #'winner-redo)))

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

;;;###autoload
(defun +other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
