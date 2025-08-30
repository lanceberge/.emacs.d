;;; -*- lexical-binding: t -*-
(use-package +window
  :ensure nil
  :bind
  (:map +leader-map
        ("nf" . #'+make-frame)
        ("bd" . #'kill-current-buffer)
        ("bq" . #'+save-and-kill-buffer)
        ("b SPC d" . #'+kill-window-and-buffer)
        ("br" . #'+revert-buffer)
        ("bl" . #'+switch-to-recent-file)
        ("bn" . #'next-buffer)
        ("bp" . #'previous-buffer)))

(use-package window
  :ensure nil
  :bind
  (:map +leader-map
        ("wo" . #'delete-other-windows)
        ("wd" . #'delete-window)
        ("wj" . #'other-window)
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
        ("M-o" . #'ace-window)))

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
  :ensure nil
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
(defun +switch-to-recent-file ()
  "Switch to the most recent open buffer in the same vc-root-dir as the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer))
        (project-root-dir (project-root (project-current t))))
    (defun +switch-to-recent-file-helper (files)
      (if (not files)
          (message "No other recent files open in buffers")
        (let ((file (car files)))
          (if (and (get-file-buffer file)
                   (not (eq (get-file-buffer file) current-buffer))
                   (or (not project-root-dir)
                       (string-prefix-p project-root-dir file t)))
              (switch-to-buffer (get-file-buffer file))
            (+switch-to-recent-file-helper (cdr files))))))
    (+switch-to-recent-file-helper recentf-list)))
