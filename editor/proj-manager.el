;;; -*- lexical-binding: t -*-
(use-package +proj-manager
  :ensure nil
  :hook
  (window-configuration-change . +move-buffer-to-tab)
  :bind
  (:map +leader-map
        ("SPC p" . #'+project-ripgrep)
        ("pp" . #'+project-switch)
        ("onf" . #'+org-roam-file-find)
        ("l" . #'+switch-to-other-project-buffer)
        ("fp" . #'+find-package)))

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
    (let* ((current-proj (project-current nil))
           (proj-root (when current-proj (project-root current-proj)))
           (filename (buffer-file-name)))
      (when proj-root
        (let ((tab-name (file-name-nondirectory (directory-file-name (expand-file-name proj-root)))))
          (when (not (string= (alist-get 'name (tab-bar--current-tab)) tab-name))
            (when filename
              (kill-buffer (get-file-buffer filename))
              (+open-tab-if-exists tab-name)
              (find-file filename))))))))

(defun +switch-to-other-project-buffer ()
  "Switch to the most recent open buffer in the same vc-root-dir as the current buffer.
Recurse through the buffer-list but skipping the first value since that's the current buffer."
  (interactive)
  (require 'project)
  (let ((current-buffer (current-buffer))
        (project-root-dir (expand-file-name (project-root (project-current t)))))
    (defun +switch-to-recent-buffer-helper (buffer-list)
      (if (not buffer-list)
          (message "No other recent files open in buffers")
        (let ((buffer (car buffer-list)))
          (if (or (not project-root-dir)
                  (string-prefix-p project-root-dir (buffer-file-name buffer) t))
              (switch-to-buffer buffer)
            (+switch-to-recent-buffer-helper (cdr buffer-list))))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)))))
