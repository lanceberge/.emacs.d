;;; -*- lexical-binding: t -*-
(use-package project
  :defer 0.1
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-commands #'project-find-file)
  :general
  (my-leader-def
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "pe" #'(flymake-show-project-diagnostics :which-key "show errors")
    "pr" #'+project-replace-regex
    "SPC r" #'project-query-replace-regexp
    "pt" (defun +project-find-todos ()
           (interactive)
           (consult-ripgrep (project-root (project-current t)) "TODO"))
    "pv" #'(+project-switch-and-vterm :which-key "switch and vterm")
    "rp" #'+project-load-projects)
  :config
  (+project-load-projects))

;;;###autoload
(defun +project-switch-and-vterm ()
  (interactive)
  (let ((project-switch-commands #'+vterm-project))
    (call-interactively 'project-switch-project)))

;;;###autoload
(defun +project-load-projects ()
  (interactive)
  (dolist (dir '("~/.emacs.d" "~/.config/nixos" "~/dotfiles"))
    (when (file-directory-p dir)
      (project--remember-dir dir)))
  (dolist (dir '("~/code/git" "~/code/phoenix" "~/code/laravel" "~/code/"))
    (when (file-directory-p dir)
      (project-remember-projects-under dir))))

;;;###autoload
(defun +project-replace-regex (search-regex replace-string)
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")))
  (let* ((project-root (vc-root-dir))
         (default-directory project-root))
    (if project-root
        (progn
          (shell-command
           (format "git ls-files -z | xargs -0 perl -pi -e 's/%s/%s/g'"
                   (shell-quote-argument search-regex)
                   (shell-quote-argument replace-string)))
          (message (format "Replaced")))
      (message "fatal: not in a git repository"))))
