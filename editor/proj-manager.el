;;; -*- lexical-binding: t -*-
(use-package +proj-manager
  :ensure nil
  :bind
  (:map +leader-map
        ("SPC p" . #'+project-ripgrep)
        ("pp" . #'+project-switch)
        ("pk" . #'+project-kill-buffers)
        ("j" . #'+project-other-project)
        ("k" . #'+switch-to-project)
        ("l" . #'+project-other-buffer)
        ("bl" . #'+project-other-special-buffer-dwim)
        ("fp" . #'+find-package)))

;;;###autoload
(defun +project-switch (&optional dir callback)
  (interactive)
  (let* ((dir (or dir (funcall project-prompter)))
         (callback (or callback #'project-switch-project)))
    (funcall callback dir)))

;;;###autoload
(defun +find-package ()
  (interactive)
  (consult-ripgrep "~/.emacs.d/" "use-package "))

;;;###autoload
(defun +pull-repos ()
  (interactive)
  (let ((pull-repos-cmd "~/dotfiles/bin/pull_repos"))
    (if (file-exists-p pull-repos-cmd)
        (shell-command pull-repos-cmd)
      (message "pull_repos doesn't exist")))
  (yas-reload-all)
  (org-roam-db-sync)
  (+source-init-file))

;;;###autoload
(defun +project-ripgrep ()
  (interactive)
  (+project-switch nil #'consult-ripgrep))

;;;###autoload
(defun +project-other-buffer (n &optional project not-found-callback)
  "Switch to the N'th most recent open buffer in the same vc-root-dir as the current buffer.
Recurse through the buffer-list, skipping the first value since that's the current buffer."
  (interactive "p")
  (require 'project)
  (let ((current-buffer (current-buffer))
        (project-root-dir
         (expand-file-name
          (or project
              (when (project-current t)
                (project-root (project-current t))))))
        (target-index (1- (abs n)))) ; Convert to 0-based index
    (defun +switch-to-recent-buffer-helper (buffer-list remaining)
      (if (not buffer-list)
          (if not-found-callback
              (funcall not-found-callback)
            (message "No other recent files open in buffers"))
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (buffer-file-name buffer)
                   (or (not project-root-dir)
                       (string= project-root-dir buffer-project-root-dir)))
              (if (eq remaining 0)
                  (switch-to-buffer buffer)
                (+switch-to-recent-buffer-helper (cdr buffer-list) (1- remaining)))
            (+switch-to-recent-buffer-helper (cdr buffer-list) remaining)))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)) target-index)))

;;;###autoload
(defun +switch-to-project (n)
  (interactive "p")
  (let ((dir (funcall project-prompter)))
    (+project-other-buffer n dir (lambda () (project-switch-project dir)))))

(defun +project-other-project (n)
  "Switch to the buffer in the last open project"
  (interactive "p")
  (require 'project)
  (let ((current-buffer (current-buffer))
        (project-root-dir (when (project-current t)
                            (expand-file-name (project-root (project-current t)))))
        (target-index (1- (abs n)))) ; Convert to 0-based index
    (defun +switch-to-recent-buffer-helper (buffer-list remaining)
      (if (not buffer-list)
          (message "No other recent files open in buffers")
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (buffer-file-name buffer)
                   (or (not project-root-dir)
                       (not (string= project-root-dir buffer-project-root-dir))))
              (if (eq remaining 0)
                  (switch-to-buffer buffer)
                (+switch-to-recent-buffer-helper (cdr buffer-list) (1- remaining)))
            (+switch-to-recent-buffer-helper (cdr buffer-list) remaining)))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)) target-index)))

;;;###autoload
(defun +project-other-special-buffer-dwim (arg)
  "If this is a special buffer, switch to the last real buffer. Otherwise, switch to the last special buffer."
  (interactive "p")
  (if (buffer-file-name (current-buffer))
      (+project-other-special-buffer)
    (+project-other-buffer arg)))

;;;###autoload
(defun +project-other-special-buffer ()
  "Switch to the most recent buffer with the same vc-root-dir. Unlike `+project-other-buffer',
this function allows special buffers."
  (interactive)
  (require 'project)
  (let ((current-buffer (current-buffer))
        (project-root-dir (when (project-current t)
                            (expand-file-name (project-root (project-current t))))))
    (defun +switch-to-recent-buffer-helper (buffer-list)
      (if (not buffer-list)
          (message "No other recent files open in buffers")
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (not (buffer-file-name buffer))
                   (string= project-root-dir buffer-project-root-dir)
                   (not (string-prefix-p " *Minibuf-" (buffer-name buffer))))
              (switch-to-buffer buffer)
            (+switch-to-recent-buffer-helper (cdr buffer-list))))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)))))
