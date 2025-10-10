;;; -*- lexical-binding: t -*-
(use-package magit
  :defer 1.0
  :defer-incrementally
  (magit-section dash f s with-editor git-commit package eieio lv transient)
  :custom
  (magit-save-repository-buffers nil)
  (magit-no-confirm '(stage-all-changes amend-published))
  (magit-diff-visit-prefer-worktree t)
  :hook
  (git-commit-mode . meow-insert-mode)
  :bind
  (:map with-editor-mode-map
        ([remap delete-window] . #'with-editor-cancel)
        ([remap save-buffer] . #'with-editor-finish))
  (:map magit-diff-section-map
        ("C-j" . #'magit-section-forward))
  (:map magit-status-mode-map
        ("C-j" . #'magit-section-forward)
        ("C-k" . #'magit-section-backward)
        ("M-j" . #'magit-jump-to-staged)
        ("M-k" . #'magit-jump-to-unstaged)
        ("q" . #'magit-commit)
        ("p" . #'magit-push)
        ("x" . #'magit-discard)
        ("V" . #'set-mark-command)
        ([remap meow-next] . #'magit-next-line)
        ([remap meow-prev] . #'magit-previous-line)
        ([remap meow-line] . #'magit-discard)
        ([remap meow-right] . #'magit-log-head)
        ([remap meow-right-expand] . #'magit-log))
  (:map +leader-map
        ("gs" . #'magit-status)
        ("gb" . #'magit-branch-or-checkout)
        ("gd" . #'magit-file-delete)
        ("SPC gf" . #'magit-fetch)
        ("SPC gb" . #'magit-branch-and-checkout)
        ("gc" . #'magit-branch-spinoff)
        ;; ("gnf" . #'magit-commit-fixup)
        ("gi" . #'magit-init)
        ("gl" . #'magit-log-head)
        ("SPC gl" . #'magit-log)
        ("gf" . #'magit-find-file)
        ("gw" . #'magit-worktree)
        ("gh" . #'+magit-diff-head-n))
  :config
  (cl-loop for n from 1 to 9
           do (let ((key (number-to-string n))
                    (desc (format "Diff HEAD~%d" n)))
                (transient-append-suffix 'magit-diff "d"
                  `(,key ,desc (lambda () (interactive) (+magit-diff-head-n ,n))))))
  (setq magit-auto-revert-mode nil))

;;;###autoload
(defun +magit-diff-head-n (&optional arg)
  "Show the diff of HEAD and the previous `n' commits. Prompts for `n'
unless a nonzero and non-negative prefix is provided."
  (interactive "p")
  (magit-diff-range (format "HEAD~%d" arg)))

(use-package git-timemachine
  :commands (git-timemachine)
  :hook (git-timemachine-mode . +meow-motion-mode)
  :bind
  (:repeat-map git-timemachine-repeat-map
               ("p" . #'git-timemachine-show-previous-revision)
               ("n" . #'git-timemachine-show-next-revision)
               :exit
               ("q" . #'git-timemachine-quit))
  (:map +leader-map
        ("gt" . #'+git-timemachine))
  (:map git-timemachine-mode-map
        ([remap meow-quit] . #'git-timemachine-quit)))

;;;###autoload
(defun +git-timemachine ()
  (interactive)
  (git-timemachine)
  (set-transient-map git-timemachine-repeat-map t))

(use-package diff-hl
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :defer 0.5
  :config
  (global-diff-hl-mode))

(use-package git-link
  :custom
  (git-link-use-commit t)
  :bind
  (:map +leader2-map
        ("gl" . #'git-link)))

(use-package +git
  :ensure nil
  :bind
  (:map +leader2-map
        ("gf" . #'git-modified-files)))

(defun git-modified-files ()
  "Display modified git files in the minibuffer."
  (interactive)
  (let* ((default-directory (or (vc-git-root default-directory)
                                (error "Not in a Git repository")))
         (output (shell-command-to-string
                  "git status --porcelain | grep -E '^[AM][ M]?|^[ M][ M]|^\\?\\?' | awk '{print $2}'"))
         (files (split-string (string-trim output) "\n" t))
         (completion-table (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata (category . file))
                               (complete-with-action action files string pred)))))
    (if (null files)
        (message "No modified, new, or staged files found.")
      (let ((selected-file (completing-read "Select modified git file: " completion-table nil t)))
        (when selected-file
          (find-file selected-file))))))
