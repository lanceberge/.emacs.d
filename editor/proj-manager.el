;;; -*- lexical-binding: t -*-
(use-package +proj-manager
  :ensure nil
  :hook
  (window-configuration-change . +move-buffer-to-tab)
  :bind
  (:map +leader-map
        ("SPC p" . #'+project-ripgrep)
        ("pp" . #'+project-switch)
        ("pk" . #'+project-kill-buffers)
        ("onf" . #'+org-roam-file-find)
        ("j" . #'+switch-to-other-project)
        ("pg" . #'+switch-to-project)
        ("l" . #'+switch-to-other-project-file-buffer)
        ("bl" . #'+switch-to-other-project-special-buffer-dwim)
        ("fp" . #'+find-package)))

;; TODO add interactive params for the tab name and add to embark-tab-map
;;;###autoload
(defun +project-kill-buffers ()
  (interactive)
  (let* ((dir (funcall project-prompter))
         (tab-name (+proj-tab-name dir)))
    (project-kill-buffers t (+vc-project-info dir))
    (tab-bar-close-tab-by-name tab-name)))

;;;###autoload
(defun +vc-project-info (dir)
  "Return VC project info for DIR in the format (vc BACKEND DIR)."
  (let ((expanded-dir (expand-file-name dir)))
    (when-let ((backend (vc-responsible-backend expanded-dir)))
      (list 'vc backend expanded-dir))))

;;;###autoload
(defun +project-switch (&optional dir callback)
  (interactive)
  (let* ((dir (or dir (funcall project-prompter)))
         (callback (or callback #'project-switch-project)))
    (let ((tab-name (file-name-nondirectory (directory-file-name (expand-file-name dir)))))
      (+open-tab-if-exists tab-name)
      (funcall callback dir))))

;;;###autoload
(defun +org-roam-file-find ()
  (interactive)
  (+open-tab-if-exists "org-roam")
  (call-interactively #'consult-org-roam-file-find))

;;;###autoload
(defun +find-package ()
  (interactive)
  (+open-tab-if-exists ".emacs.d")
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
(defun +move-buffer-to-tab ()
  "Move the current buffer to the tab corresponding to it's vc root."
  (interactive)
  (require 'project)
  (unless (or (minibufferp) (window-minibuffer-p))
    (let* ((tab-name (+current-proj-tab-name))
           (filename (buffer-file-name)))
      (when (and tab-name
                 (not (string= (alist-get 'name (tab-bar--current-tab)) tab-name))
                 filename)
        (kill-buffer (get-file-buffer filename))
        (+open-tab-if-exists tab-name)
        (find-file filename)))))

;;;###autoload
(defun +proj-tab-name (proj-root)
  (file-name-nondirectory (directory-file-name (expand-file-name proj-root))))

;;;###autoload
(defun +current-proj-tab-name ()
  (let* ((current-proj (project-current nil))
         (proj-root (when current-proj (project-root current-proj))))
    (when proj-root
      (+proj-tab-name proj-root))))

;;;###autoload
(defun +switch-to-other-project-file-buffer (n &optional project)
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
    (message dir)
    (+switch-to-other-project-file-buffer n dir (lambda () (project-switch-project dir)))))

(defun +switch-to-other-project (n)
  "Switch to the buffer in the last open project"
  (interactive "p")
  (require 'project)
  (let ((current-buffer (current-buffer))
        >>>>>>> Stashed changes
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
                       (string= project-root-dir buffer-project-root-dir)))
              (if (eq remaining 0)
                  (switch-to-buffer buffer)
                (+switch-to-recent-buffer-helper (cdr buffer-list) (1- remaining)))
            (+switch-to-recent-buffer-helper (cdr buffer-list) remaining)))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)) target-index)))

;;;###autoload
(defun +switch-to-other-project-special-buffer-dwim (arg)
  "If this is a special buffer, switch to the last real buffer. Otherwise, switch to the last special buffer."
  (interactive "p")
  (if (buffer-file-name (current-buffer))
      (+switch-to-other-project-special-buffer)
    (+switch-to-other-project-file-buffer arg)))

;;;###autoload
(defun +switch-to-other-project-special-buffer ()
  "Switch to the most recent buffer with the same vc-root-dir. Unlike `+switch-to-other-project-file-buffer',
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
                   (string= project-root-dir buffer-project-root-dir))
              (switch-to-buffer buffer)
            (+switch-to-recent-buffer-helper (cdr buffer-list))))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)))))
