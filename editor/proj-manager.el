;;; -*- lexical-binding: t -*-
(use-package +proj-manager
  :ensure nil
  :hook
  ((prog-mode text-mode) . +move-buffer-to-tab)
  :bind
  (:map +leader-map
        ("SPC p" . #'+project-ripgrep)
        ("pp" . #'+project-switch)
        ("onf" . #'+org-roam-file-find)
        ("fp" . #'+find-package)
        ("gr" . #'+find-package)))

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
    (let ((proj-root (project-root (project-current t)))
          (filename (buffer-file-name)))
      (when proj-root
        (let ((tab-name (file-name-nondirectory (directory-file-name (expand-file-name proj-root)))))
          (when (not (string= (alist-get 'name (tab-bar--current-tab)) tab-name))
            (when filename
              (kill-buffer (get-file-buffer filename))
              (+open-tab-if-exists tab-name)
              (find-file filename))))))))
