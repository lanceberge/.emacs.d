;;; -*- lexical-binding: t -*-
(use-package project
  :demand t
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-commands #'project-find-file)
  (project-mode-line t)
  :bind
  (:map +leader-map
        ("pf" . #'project-find-file)
        ("'" . #'project-find-file)
        ("pe" . #'flymake-show-project-diagnostics)
        ("pr" . #'+project-replace-regex)
        ("pt" . #'+project-find-todos)
        ("rp" . #'+project-load-projects)))

;;;###autoload
(defun +project-find-todos ()
  (interactive)
  (consult-ripgrep (project-root (project-current t)) "TODO"))

;;;###autoload
(defun +project-load-projects ()
  (interactive)
  ;; dirs to remember
  (dolist (dir '("~/.emacs.d" "~/.config/nixos" "~/dotfiles"))
    (when (file-directory-p dir)
      (project--remember-dir dir)))
  ;; dirs to remember recursively
  (dolist (dir '("~/code/git" "~/code/phoenix" "~/code/laravel" "~/code/elixir/" "~/code/" "~/src/"))
    (when (file-directory-p dir)
      (project-remember-projects-under dir))))

;;;###autoload
(defun +project-replace-regex (search-regex replace-string)
  "Perform a replacement on all git files of SEARCH-REGEX to REPLACE-STRING."
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
  "Quote a string for safe use in Perl/shell by escaping special characters."
  (replace-regexp-in-string
   "[\"/+$&|;()<>'` \t\n\r\\\\]"
   "\\\\\\&"
   str
   t))
