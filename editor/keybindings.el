;;; -*- lexical-binding: t -*-
(use-package +keybindings
  :ensure nil
  :bind
  (:map +leader-map
        ("h" . #'help-command)
        (";" . #'shell-command)
        ("SPC ;" . #'+project-shell-command)
        ("er" . #'+source-init-file))
  (:map meow-insert-state-keymap
        ("j" . #'+escape))
  (:map meow-normal-state-keymap
        ("SPC [" . #'+insert-newlines-above)
        ("SPC ]" . #'+insert-newlines-below)
        ([remap newline] . #'insert-newline-dwim)
        ("S-<return>" . #'+split-line-above)))

;;;###autoload
(defun insert-newline-indent ()
  (interactive)
  (save-excursion
    (newline)
    (indent-for-tab-command)))

;;;###autoload
(defun +split-line-above ()
  (interactive)
  (insert-newline-indent)
  (save-excursion
    (forward-line)
    (drag-stuff-up 1)))

;;;###autoload
(defun insert-newline-dwim ()
  (interactive)
  (if (region-active-p)
      (insert-newline-preserving-region)
    (progn
      (newline)
      (indent-for-tab-command))))

;;;###autoload
(defun insert-newline-above-dwim ()
  "split the line above at point"
  (interactive)
  (let ((deactivate-mark nil))
    (if (region-active-p)
        (progn
          (insert-newline-preserving-region)
          (save-excursion
            (next-line)
            (indent-for-tab-command)))
      (progn
        (insert-newline-dwim)
        (drag-stuff-up 1)
        (indent-for-tab-command)))))

;;;###autoload
(defun insert-newline-preserving-region ()
  "Insert a newline at point while preserving the region."
  (let* ((was-active (region-active-p))
         (old-point (point))
         (old-mark (and (region-active-p) (mark t)))
         (deactivate-mark nil)
         (point-at-end (eq old-point (region-end)))
         (region-len (abs (- old-point old-mark))))
    (insert "\n")
    (indent-according-to-mode)
    (when point-at-end
      (goto-char old-point)
      (set-mark (- (point) region-len))
      (activate-mark))))

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
        ([remap scroll-up-command] . #'+rectangle-mode)) ; C-v
  (:map rectangle-mark-mode-map
        ([remap delete-char] . #'kill-rectangle)
        ([remap meow-insert] . #'string-insert-rectangle)
        ([remap +meow-change] . #'replace-rectangle)
        ([remap meow-next-word] . #'forward-word)
        ([remap meow-back-word] . #'backward-word)))

;;;###autoload
(defun +rectangle-mode ()
  (interactive)
  (rectangle-mark-mode)
  (forward-char))

;;;###autoload
(defun +insert-newlines-above (arg)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (dotimes (_ arg)
      (newline))))

;;;###autoload
(defun +insert-newlines-below (arg)
  (interactive "p")
  (save-excursion
    (end-of-line)
    (dotimes (_ arg)
      (newline))))
