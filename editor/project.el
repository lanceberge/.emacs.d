;;; -*- lexical-binding: t -*-
(use-package project
  :defer 0.1
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-commands #'project-find-file)
  :bind
  (:map +leader-map
        ("pf" . #'project-find-file)
        ("ps" . #'consult-ripgrep)
        ("pe" . #'flymake-show-project-diagnostics)
        ("pr" . #'+project-replace-regex)
        ("pt" . #'+project-find-todos)
        ("rp" . #'+project-load-projects))
  :config
  (+project-load-projects))

;;;###autoload
(defun +project-find-todos ()
  (interactive)
  (consult-ripgrep (project-root (project-current t)) "TODO"))

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
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root))
    (if project-root
        (progn (shell-command (format "git ls-files -z | xargs -0 perl -pi -e \"s/%s/%s/g\""
                                      (+perl-shell-quote search-regex)
                                      (+perl-shell-quote replace-string))
                              (message (format "Replaced"))))
      (message "fatal: not in a git repository"))))

;;;###autoload
(defun +perl-shell-quote (str)
  (let* ((str (replace-regexp-in-string "\"" "\\\\\"" str))
         (str (replace-regexp-in-string "/" "\\\\/" str)))
    str))
