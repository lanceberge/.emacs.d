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
        ("bl" . #'+switch-to-recent-project-file)
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
(defun +switch-to-recent-project-file ()
  "Switch to the most recent open buffer in the same project as the current buffer."
  (interactive)
  (if-let ((current-project-root (when-let ((proj (project-current)))
                                   (project-root proj))))
      (let ((current-buffer (current-buffer)))
        (defun +switch-to-recent-file-helper (buffer-names)
          (if (not buffer-names)
              (message "No other recent files open in buffers from the same project")
            (let* ((raw-buffer-name (car buffer-names))
                   ;; remove non-ascii characters in consult--buffer-history
                   (buffer-name (replace-regexp-in-string "[^\x20-\x7E]" "" raw-buffer-name))
                   (buffer (get-buffer buffer-name)))
              (if (and buffer
                       (not (eq buffer current-buffer))
                       (buffer-live-p buffer))
                  ;; Check if this buffer belongs to the same project
                  (if-let ((buffer-project-root
                            (with-current-buffer buffer
                              (when-let ((proj (project-current)))
                                (project-root proj)))))
                      (if (string= current-project-root buffer-project-root)
                          (switch-to-buffer buffer)
                        (+switch-to-recent-file-helper (cdr buffer-names)))
                    (+switch-to-recent-file-helper (cdr buffer-names)))
                (+switch-to-recent-file-helper (cdr buffer-names))))))
        (+switch-to-recent-file-helper consult--buffer-history))
    (message "Not in a project")))
