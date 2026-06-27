;;; -*- lexical-binding: t -*-
(use-package +window
  :ensure nil
  :bind
  (:repeat-map buffer-repeat-map
               ("n" . #'next-buffer)
               ("p" . #'previous-buffer))
  (:map +x-map
        ("1" . #'+toggle-tab-zoom))
  (:map +leader-map
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

(use-package window
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map +x-map
        ("0" . #'delete-window)
        ("2" . #'split-window-below)
        ("3" . #'split-window-right)))

(use-package winner ; Undo and redo window configs
  :ensure nil
  :hook
  (emacs-startup . winner-mode)
  :bind
  (:repeat-map winner-repeat-map
               ("p" . #'winner-undo)
               ("n" . #'winner-redo)))

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

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-name-function #'+tab-bar-tab-name-project)
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :bind
  (:map +leader-map
        ("tj" . #'tab-bar-switch-to-recent-tab)
        ("tn" . #'tab-bar-new-tab)
        ("tf" . #'tab-bar-switch-to-tab)))

;;;###autoload
(defun +tab-bar-tab-name-project ()
  "Generate tab name as PROJECT:BUFFER from the selected window's buffer."
  (let* ((win (or (minibuffer-selected-window)
                  (and (window-minibuffer-p) (get-mru-window))))
         (buf (window-buffer win))
         (name (buffer-name buf))
         (project (with-current-buffer buf (project-current))))
    (if project
        (format "%s:%s"
                (file-name-nondirectory
                 (directory-file-name (project-root project)))
                name)
      name)))

(use-package consult-tab-bar
  :ensure (:type file :main "~/.emacs.d/packages/consult-tab.el")
  :bind
  (:map +leader-map
        ("SPC t" . #'+consult-tab)))

(use-package tab-bar-repeat
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map +x-map
        ("wp" . #'tab-bar-history-back)
        ("wn" . #'tab-bar-history-forward)))

;;;###autoload
(defun +revert-buffer ()
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun +other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(use-package pulsar
  :hook
  (window-buffer-change-functions . +pulsar-pulse-line-no-minibuffer))

(defvar +pulsar--last-buffer nil
  "Buffer in the selected window the last time we pulsed.")

;;;###autoload
(defun +pulsar-pulse-line-no-minibuffer (frame-or-window)
  (let* ((win (if (framep frame-or-window)
                  (frame-selected-window frame-or-window)
                frame-or-window))
         (buf (window-buffer win)))
    (unless (or (minibufferp buf)
                (eq buf +pulsar--last-buffer))
      (setq +pulsar--last-buffer buf)
      (with-selected-window win
        (pulsar-pulse-line)))))

;;;###autoload
(defun +toggle-tab-zoom ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and tab-bar-mode
           (equal (selected-window) (next-window)))
      (tab-bar-history-back)
    ;; Force tab-bar history to record this command even if it was repeated.
    (setq tab-bar-history-done-command nil)
    (delete-other-windows)))
