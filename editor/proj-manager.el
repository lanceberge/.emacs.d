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
        ("l" . #'+switch-to-other-project-buffer)
        ("fp" . #'+find-package)))

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
(defun +switch-to-other-project-buffer ()
  "Switch to the most recent open buffer in the same vc-root-dir as the current buffer.
Recurse through the buffer-list but skipping the first value since that's the current buffer."
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
          (if (and (buffer-file-name buffer)
                   (or (not project-root-dir)
                       (string= project-root-dir buffer-project-root-dir)))
              (switch-to-buffer buffer)
            (+switch-to-recent-buffer-helper (cdr buffer-list))))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)))))
