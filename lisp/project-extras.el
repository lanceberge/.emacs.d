;;; project-extras.el --- Project navigation helpers -*- lexical-binding: t -*-

(require 'project)

;;;###autoload
(defun +project-switch-project (command dir)
  "Same as `project-switch-project' except reads the command first,
which for some reason I prefer."
  (interactive
   (let ((command (+project-switch-project-command-or-quit)))
     (list command (funcall project-prompter))))
  (project-remember-project (project-current t dir))
  (let ((project-current-directory-override dir))
    (call-interactively command)))

;;;###autoload
(defun +project-other-buffer (n &optional project)
  "Switch to the N'th most recent file-visiting buffer in PROJECT."
  (interactive "p")
  (let ((buffer (+project-other-buffer-buffer n project)))
    (if buffer
        (switch-to-buffer buffer)
      (call-interactively #'project-find-file))))

;;;###autoload
(defun +project-kill-buffer ()
  "Kill the current buffer and switch to the most
recent buffer in the same project."
  (interactive)
  (let* ((buffer (current-buffer))
         (project (project-current nil))
         (project-root-dir (when project
                             (expand-file-name (project-root project)))))
    (if-let ((other-buffer (and project-root-dir
                                (+project-other-buffer-buffer 1 project-root-dir))))
        (progn
          (switch-to-buffer other-buffer)
          (kill-buffer buffer))
      (kill-current-buffer))))

;;;###autoload
(defun +project-other-buffer-buffer (n &optional project)
  "Return the N'th most recent file-visiting buffer in PROJECT."
  (let* ((project (cond
                   ((stringp project) (project-current t project))
                   (project)
                   ((project-current nil))))
         (buffers (and project
                       (seq-remove
                        (lambda (buffer)
                          (or (eq buffer (current-buffer))
                              (not (buffer-file-name buffer))))
                        (project-buffers project)))))
    (nth (1- (abs n)) buffers)))

;;;###autoload
(defun +project-visit-last-buffer (n &optional dir)
  "Switch to the last open buffer in a project at `DIR'."
  (interactive "p")
  (let ((dir (or dir (funcall project-prompter))))
    (+project-other-buffer n dir)))

;;;###autoload
(defun +project-other-project-command (command dir)
  "Run a project command in the most recently opened other project."
  (interactive
   (let* ((dir (+project-last-opened-other-project-root))
          (command (+project-switch-project-command-or-quit dir)))
     (list command dir)))
  (project-remember-project (project-current t dir))
  (let ((project-current-directory-override dir))
    (call-interactively command)))

;;;###autoload
(defun +project-switch-project-command-or-quit (&optional dir)
  "Read a project command for DIR, quitting immediately on quit commands."
  (let ((command (project--switch-project-command dir)))
    (when (memq command '(keyboard-quit keyboard-escape-quit))
      (keyboard-quit))
    command))

;;;###autoload
(defun +project-last-opened-other-project-root ()
  "Return the project root for the most recent buffer in another project."
  (let* ((current (when-let ((project (project-current nil)))
                    (file-truename (project-root project))))
         (dir (seq-some
               (lambda (buffer)
                 (when-let* (((buffer-file-name buffer))
                             (root (with-current-buffer buffer
                                     (when-let ((project (project-current nil)))
                                       (expand-file-name (project-root project)))))
                             ((or (not current)
                                  (not (string= (file-truename root) current)))))
                   root))
               (cdr (buffer-list)))))
    (or dir
        (funcall project-prompter))))

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
  (let* ((project (project-current t))
         (buffer (seq-find
                  (lambda (buffer)
                    (and (not (eq buffer (current-buffer)))
                         (not (buffer-file-name buffer))
                         (not (string-prefix-p " *Minibuf-" (buffer-name buffer)))))
                  (project-buffers project))))
    (if buffer
        (switch-to-buffer buffer)
      (call-interactively #'project-find-file))))


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
  (call-interactively #'+project-switch-project))

;;;###autoload
(defun +project-load-projects ()
  (interactive)
  (project-forget-zombie-projects)
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

;; TODO
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

(provide 'project-extras)
;;; project-extras.el ends here
