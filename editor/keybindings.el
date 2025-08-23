;;; -*- lexical-binding: t -*-
(use-package emacs
  :ensure nil
  :general
  (my-leader-def
    "nf" (defun +make-frame ()
           (interactive)
           (let ((frame (make-frame)))
             (when (and IS-LINUX (>= emacs-major-version 29))
               (set-frame-parameter frame 'undecorated t))))
    "h" #'help-command
    ":" #'shell-command
    ";" #'(lambda ()
            (interactive)
            (let ((default-directory (project-root (project-current t))))
              (call-interactively #'shell-command)))

    ;; Buffers
    "bd" #'kill-current-buffer
    "bq" #'+save-and-kill-buffer
    "b SPC d" #'+kill-window-and-buffer
    "br" (defun +revert-buffer () (interactive)
                (revert-buffer t t))
    "bl" #'+switch-to-recent-file
    "bn" #'next-buffer
    "bp" #'previous-buffer

    "er" (defun +source-init-file () (interactive)
                (load-file "~/.emacs.d/init.el")))

  ('meow-insert-state-keymap
   "j" #'+escape))

;; TODO
;;;###autoload
(defun +append-semicolon ()
  (interactive)
  save-excursion
  (end-of-line)
  (unless (looking-back ";" nil)
    (Insert ";")))

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
