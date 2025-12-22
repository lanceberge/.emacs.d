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
  (project-forget-zombie-projects)
  ;; dirs to remember
  (dolist (dir '("~/.emacs.d/" "~/.config/nixos" "~/dotfiles/"))
    (when (file-directory-p dir)
      (project--remember-dir dir)))
  ;; dirs to remember recursively
  (dolist (dir '("~/code/git" "~/code/phoenix" "~/code/vaultvantage/" "~/code/laravel" "~/code/elixir/" "~/code/" "~/src/"))
    (when (file-directory-p dir)
      (project-remember-projects-under dir))))

;;;###autoload
(defun +project-replace-regex (search-regex replace-string &optional file-pattern)
  "Perform a replacement on all git files of SEARCH-REGEX to REPLACE-STRING.
If FILE-PATTERN is provided (e.g. \"*.ex\"), only match files with that pattern."
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")
                     (read-string "File pattern (optional, e.g. *.ex): " nil nil "")))
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (file-filter (if file-pattern
                          (concat " " (shell-quote-argument file-pattern))
                        ""))
         (cmd (format "git ls-files -z --cached --others --exclude-standard%s | xargs -0 perl -pi -e \"s/%s/%s/g\""
                      file-filter
                      (+perl-regex-shell-quote search-regex)
                      (+perl-replacement-shell-quote replace-string))))
    (if project-root
        (progn
          (shell-command cmd))
      (message "fatal: not in a git repository"))))

;;;###autoload
(defun +perl-regex-shell-quote (str)
  "Quote a regex string for Perl, preserving regex backslashes but escaping shell chars."
  (replace-regexp-in-string
   "[\"/`$]"
   "\\\\\\&"
   str
   t))

;;;###autoload
(defun +perl-replacement-shell-quote (str)
  "Quote a replacement string for Perl/shell."
  (replace-regexp-in-string
   "[\"/+$&|;()<>'` \t\n\r\\\\]"
   "\\\\\\&"
   str
   t))
