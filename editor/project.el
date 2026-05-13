;;; -*- lexical-binding: t -*-
(require 'subr-x)

(use-package project
  :demand t
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-commands #'project-find-file)
  (project-mode-line t)
  :bind
  (:map +leader-map
        ("t" . #'+tmux-sessionizer-current-directory)
        ("'" . #'project-find-file)
        ("pe" . #'flymake-show-project-diagnostics)
        ("pr" . #'+project-replace-regex)
        ("pt" . #'+project-find-todos)
        ("rk" . #'+project-reload-and-switch)
        ("rp" . #'+project-load-projects)))

;;;###autoload
(defun +project-find-todos ()
  (interactive)
  (consult-ripgrep (project-root (project-current t)) "TODO"))

;;;###autoload
(defun +tmux-sessionizer-current-directory ()
  "Open the current git root or directory in the workspace-1 Alacritty tmux client."
  (interactive)
  (let* ((dir (file-name-as-directory
               (expand-file-name
                (or (and buffer-file-name (file-name-directory buffer-file-name))
                    default-directory))))
         (target-dir (file-name-as-directory
                      (or (locate-dominating-file dir ".git")
                          dir)))
         (script (expand-file-name "~/dotfiles/bin/hypr_tmux_sessionizer"))
         (buffer (get-buffer-create "*hypr_tmux_sessionizer*")))
    (unless (file-executable-p script)
      (user-error "Cannot execute %s" script))
    (with-current-buffer buffer
      (erase-buffer))
    (let ((proc (make-process
                 :name "hypr_tmux_sessionizer"
                 :buffer buffer
                 :command (list script target-dir)
                 :noquery t)))
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let ((output (string-trim
                          (with-current-buffer (process-buffer process)
                            (buffer-string)))))
             (if (= 0 (process-exit-status process))
                 (message "Opened tmux session for %s" target-dir)
               (message "hypr_tmux_sessionizer failed: %s" output))))))
      (message "Opening tmux session for %s" target-dir))))

;;;###autoload
(defun +project-reload-and-switch ()
  (interactive)
  (+project-load-projects)
  (+switch-to-project 1))

;;;###autoload
(defun +kill-zombie-buffers ()
  (interactive)
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (let ((file-name (buffer-file-name buffer)))
        (when (and file-name (not (file-exists-p file-name)))
          (kill-buffer buffer)
          (setq buffers-killed (+ 1 buffers-killed)))))
    (message "Killed %d non-existent file buffer(s)." buffers-killed)))

;;;###autoload
(defun +project-load-projects ()
  (interactive)
  (project-forget-zombie-projects)
  (+kill-zombie-buffers)
  ;; dirs to remember
  (dolist (dir '("~/.emacs.d/" "~/.config/nixos" "~/dotfiles/" ))
    (when (file-directory-p dir)
      (project--remember-dir dir)))

  ;; remember all projects under dirs (non-recursive)
  (dolist (dir '("~/code/git" "~/code/phoenix" "~/code/vaultvantage/" "~/worktrees/" "~/code/laravel" "~/code/elixir/" "~/code/" "~/src/" "~/worktrees/vault-landing-page/orchestrator/"))
    (when (file-directory-p dir)
      (project-remember-projects-under dir)))

  (let ((worktree-dir (expand-file-name "~/jj-workspaces/")))
    (when (file-directory-p worktree-dir)
      (dolist (subdir (directory-files worktree-dir t "^[^.]"))
        (when (file-directory-p subdir)
          (project-remember-projects-under subdir)))))
  )

;;;###autoload
(defun +project-replace-regex (search-regex replace-string &optional file-pattern)
  "Perform a replacement on all git files of SEARCH-REGEX to REPLACE-STRING.
If FILE-PATTERN is provided (e.g. \"*.ex\"), only match files with that pattern."
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")
                     (read-string "File pattern (optional, e.g. *.ex): " nil nil ".")))
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
