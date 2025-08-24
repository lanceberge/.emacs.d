;;; -*- lexical-binding: t -*-
(use-package emacs
  :ensure nil
  :bind
  (:map +leader-map
        ("nf" . #'+make-frame)
        ("h" . #'help-command)
        (";" . #'shell-command)
        ("SPC ;" . #'+project-shell-command)
        ("bd" . #'kill-current-buffer)
        ("bq" . #'+save-and-kill-buffer)
        ("b SPC d" . #'+kill-window-and-buffer)
        ("br" . #'+revert-buffer)
        ("bl" . #'+switch-to-recent-file)
        ("bn" . #'next-buffer)
        ("bp" . #'previous-buffer)
        ("er" . #'+source-init-file))
  (:map meow-insert-state-keymap
        ("j" . #'+escape)))

;;;###autoload
(defun +project-shell-command ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))
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
(defun +source-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; TODO
;;;###autoload
(defun +append-semicolon ()
  (interactive)
  save-excursion
  (end-of-line)
  (unless (looking-back ";" nil)
    (insert ";")))

;;;###autoload
(defun +switch-to-recent-file ()
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

;;;###autoload
(defun +escape (&optional count)
  (interactive)
  (let ((cooldown 0.5))
    (let ((char (read-char nil nil cooldown)))
      (if char
          (let* ((str (char-to-string char))
                 (command (key-binding str)))
            (if (= char ?k)
                (meow-insert-exit)
              (progn
                (insert-char ?j)
                (cond
                 ((eq command #'self-insert-command)
                  (insert-char char))
                 ((eq command #'org-self-insert-command)
                  (insert-char char))))))
        (insert-char ?j)))))

(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :config
  (keyfreq-autosave-mode))
