;;; -*- lexical-binding: t -*-
(use-package project
  :commands (project-switch-project)
  :general
  (my-leader-def
    "pp" #'(+project-switch-and-find-file :which-key "switch project")
    "pg" #'(+project-switch-and-magit-status :which-key "switch project")
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "pe" #'(flymake-show-project-diagnostics :which-key "show errors")
    "pr" #'project-query-replace-regexp
    "p SPC p" #'(+project-switch-and-rg :which-key "switch project")))

;;;###autoload
(defun +project-switch-and-rg ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'consult-ripgrep)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-find-file ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'project-find-file)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-magit-status ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'magit-project-status)
  (call-interactively 'project-switch-project))
