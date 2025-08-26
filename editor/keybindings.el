;;; -*- lexical-binding: t -*-
(use-package +essentials
  :ensure nil
  :bind
  (:map +leader-map
        ("h" . #'help-command)
        (";" . #'shell-command)
        ("SPC ;" . #'+project-shell-command)
        ("er" . #'+source-init-file))
  (:map meow-insert-state-keymap
        ("j" . #'+escape)))

;;;###autoload
(defun +project-shell-command ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))

;;;###autoload
(defun +source-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (looking-back ";" nil)
      (insert ";"))))

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

(use-package rect
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ([remap scroll-up-command] . #'rectangle-mark-mode)) ; C-v
  (:map rectangle-mark-mode-map
        ([remap delete-char] . #'kill-rectangle)))
