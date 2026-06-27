;;; project-extensions.el --- Project navigation helpers -*- lexical-binding: t -*-

(require 'project)

;;;###autoload
(defun +project-switch (&optional dir callback)
  (interactive)
  (let* ((dir (or dir (funcall project-prompter)))
         (default-directory dir)
         (callback (or callback #'project-switch-project)))
    (funcall callback dir)))

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
(defun +project-switch-ripgrep ()
  (interactive)
  (+project-switch nil #'consult-ripgrep))

;;;###autoload
(defun +project-switch-eshell ()
  (interactive)
  (+project-switch nil #'eshell))

;;;###autoload
(defun +project-other-buffer (n &optional project not-found-callback)
  "Switch to the N'th most recent open buffer in the same vc-root-dir as the current buffer.
Recurse through the buffer-list, skipping the first value since that's the current buffer."
  (interactive "p")
  (let ((current-buffer (current-buffer))
        (project-root-dir
         (when-let ((project (or project (project-current nil))))
           (expand-file-name
            (if (stringp project)
                project
              (project-root project)))))
        (target-index (1- (abs n)))) ; Convert to 0-based index
    (defun +switch-to-recent-buffer-helper (buffer-list remaining)
      (if (not buffer-list)
          (if not-found-callback
              (funcall not-found-callback)
            (message "No other recent files open in buffers"))
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (buffer-file-name buffer)
                   (or (not project-root-dir)
                       (string= project-root-dir buffer-project-root-dir)))
              (if (eq remaining 0)
                  (switch-to-buffer buffer)
                (+switch-to-recent-buffer-helper (cdr buffer-list) (1- remaining)))
            (+switch-to-recent-buffer-helper (cdr buffer-list) remaining)))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)) target-index)))

;;;###autoload
(defun +project-kill-buffer ()
  "Kill the current buffer and switch to the most recent buffer in the same project."
  (interactive)
  (let ((buffer (current-buffer))
        (project-root-dir (when (project-current t)
                            (expand-file-name (project-root (project-current t))))))
    (+project-other-buffer 1 project-root-dir
                           (lambda ()
                             (switch-to-buffer (other-buffer buffer t))))
    (unless (eq buffer (current-buffer))
      (kill-buffer buffer))))

;;;###autoload
(defun +project-visit-last-buffer (n &optional dir)
  "Switch to the last open buffer in a project at `DIR'."
  (interactive "p")
  (let ((dir (or dir (funcall project-prompter))))
    (+project-other-buffer n dir (lambda () (project-switch-project dir)))))

;;;###autoload
(defun +project-other-project (n)
  "Switch to the buffer in the last open project"
  (interactive "p")
  (let ((current-buffer (current-buffer))
        (project-root-dir (when-let ((project (project-current nil)))
                            (expand-file-name (project-root project))))
        (target-index (1- (abs n)))) ; Convert to 0-based index
    (defun +switch-to-recent-buffer-helper (buffer-list remaining)
      (if (not buffer-list)
          (message "No other recent files open in buffers")
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (buffer-file-name buffer)
                   (or (not project-root-dir)
                       (not (string= project-root-dir buffer-project-root-dir))))
              (if (eq remaining 0)
                  (switch-to-buffer buffer)
                (+switch-to-recent-buffer-helper (cdr buffer-list) (1- remaining)))
            (+switch-to-recent-buffer-helper (cdr buffer-list) remaining)))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)) target-index)))

;;;###autoload
(defun +project-other-special-buffer-dwim (arg)
  "If this is a special buffer, switch to the last real buffer. Otherwise, switch to the last special buffer."
  (interactive "p")
  (if (buffer-file-name (current-buffer))
      (+project-other-special-buffer)
    (+project-other-buffer arg)))

;;;###autoload
(defun +project-other-special-buffer ()
  "Switch to the most recent buffer with the same vc-root-dir. Unlike `+project-other-buffer',
this function allows special buffers."
  (interactive)
  (let ((current-buffer (current-buffer))
        (project-root-dir (when (project-current t)
                            (expand-file-name (project-root (project-current t))))))
    (defun +switch-to-recent-buffer-helper (buffer-list)
      (if (not buffer-list)
          (message "No other recent files open in buffers")
        (let* ((buffer (car buffer-list))
               (buffer-project-root-dir (with-current-buffer buffer
                                          (when (project-current nil)
                                            (expand-file-name (project-root (project-current nil)))))))
          (if (and (not (buffer-file-name buffer))
                   (string= project-root-dir buffer-project-root-dir)
                   (not (string-prefix-p " *Minibuf-" (buffer-name buffer))))
              (switch-to-buffer buffer)
            (+switch-to-recent-buffer-helper (cdr buffer-list))))))
    (+switch-to-recent-buffer-helper (cdr (buffer-list)))))


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
         (script (expand-file-name "~/dotfiles/bin/hypr/hypr_tmux_sessionizer"))
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
  (+project-visit-last-buffer 1))

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
          (project-remember-projects-under subdir))))))

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

(provide 'project-extensions)
;;; project-extensions.el ends here
