;;; -*- lexical-binding: t -*-
(use-package project
  :commands (project-switch-project)
  :custom
  (project-switch-commands #'project-find-file)
  :general
  (my-leader-def
    "pp" #'(+project-switch-and-find-file :which-key "switch project")
    "pg" #'(+project-switch-and-magit-status :which-key "switch project")
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "pe" #'(flymake-show-project-diagnostics :which-key "show errors")
    "pr" #'project-query-replace-regexp
    "pt" (defun +project-find-todos ()
           (interactive)
           (consult-ripgrep (project-root (project-current t)) "TODO"))
    "SPC p" #'(+project-switch-and-rg :which-key "switch project")
    "pv" #'(+project-switch-and-vterm :which-key "switch and vterm")))

;;;###autoload
(defun +project-switch-and-rg ()
  (interactive)
  (setq project-switch-commands #'consult-ripgrep)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-find-file ()
  (interactive)
  (setq project-switch-commands #'project-find-file)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-magit-status ()
  (interactive)
  (setq project-switch-commands #'magit-project-status)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-vterm ()
  (interactive)
  (setq project-switch-commands #'+vterm-project)
  (call-interactively 'project-switch-project))
