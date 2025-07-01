;;; -*- lexical-binding: t -*-
(use-package project
  :commands
  (project-switch-project project-prompter)
  :custom
  (project-switch-commands #'project-find-file)
  :general
  (my-leader-def
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "pe" #'(flymake-show-project-diagnostics :which-key "show errors")
    "pr" #'project-query-replace-regexp
    "pt" (defun +project-find-todos ()
           (interactive)
           (consult-ripgrep (project-root (project-current t)) "TODO"))
    "pv" #'(+project-switch-and-vterm :which-key "switch and vterm"))
  :config
  (dolist (dir '("~/code/" "~/code/phoenix" "~/code/laravel"))
    (when (file-directory-p dir)
      (project-remember-projects-under dir))))

;;;###autoload
(defun +project-switch-and-vterm ()
  (interactive)
  (setq project-switch-commands #'+vterm-project)
  (call-interactively 'project-switch-project))
