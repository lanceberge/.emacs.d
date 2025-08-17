;;; -*- lexical-binding: t -*-

;;;###autoload
(defun +open-tab-if-exists (tab-name)
  (let ((tabs (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
    (if (member tab-name tabs)
        (tab-bar-switch-to-tab tab-name)
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab tab-name)))))

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

(defun +move-file-to-tab ()
  (interactive)
  (unless (or (minibufferp) (window-minibuffer-p))
    ;; TODO misc?
    (let ((proj-root (vc-root-dir))
          (filename (buffer-file-name)))
      (when proj-root
        (let ((tab-name (file-name-nondirectory (directory-file-name (expand-file-name proj-root)))))
          (when (not (string= (alist-get 'name (tab-bar--current-tab)) tab-name))
            ;; TODO can't kill immutable buffers
            (kill-buffer (get-file-buffer filename))
            (+open-tab-if-exists tab-name)
            (find-file filename)))))))

(add-hook 'window-configuration-change-hook #'+move-file-to-tab)

(my-leader-def
  :states 'normal
  "SPC p" #'(lambda () (interactive) (+project-switch nil #'consult-ripgrep))
  "pg" #'(lambda () (interactive) (+project-switch nil #'magit-status))
  "pp" #'+project-switch
  "onf" #'+org-roam-file-find
  "fp" #'+find-package
  "gr" #'+pull-repos)
