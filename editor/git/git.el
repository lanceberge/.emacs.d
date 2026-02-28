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
  (git-commit-mode . (lambda () (+insert-mode 1)))
  :bind
  (:map with-editor-mode-map
        ([remap delete-window] . #'with-editor-cancel)
        ([remap save-buffer] . #'with-editor-finish))
  (:map magit-blob-mode-map
        ("q" . #'quit-window)
        ([remap save-buffer] . #'quit-window)
        ([remap +elisp-format-and-check] . #'quit-window))
  (:map magit-diff-section-map
        ("C-j" . #'magit-section-forward))
  (:map magit-status-mode-map
        ("C-j" . #'magit-section-forward)
        ("C-k" . #'magit-section-backward)
        ("M-j" . #'magit-jump-to-staged)
        ("M-k" . #'magit-jump-to-unstaged)
        ("q" . #'magit-commit)
        ;; ("p" . #'magit-push)
        ("x" . #'magit-discard)
        ("V" . #'set-mark-command))
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
        ("gm" . #'+magit-find-current-file-on-default-branch)
        ("gw" . #'magit-worktree)
        ("gh" . #'+magit-diff-head-n))
  :config
  (defun +magit-status-setup-motion-keys ()
    "Override motion-mode keys for magit-status."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map +motion-mode-map)
      (define-key map "j" #'magit-next-line)
      (define-key map "k" #'magit-previous-line)
      (define-key map "m" #'magit-discard)
      (define-key map "l" #'magit-log-head)
      (define-key map "L" #'magit-log)
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons '+motion-mode map)
                        minor-mode-overriding-map-alist))))
  (add-hook 'magit-status-mode-hook #'+magit-status-setup-motion-keys)

  (cl-loop for n from 1 to 9
           do (let ((key (number-to-string n))
                    (desc (format "Diff HEAD~%d" n)))
                (transient-append-suffix 'magit-diff "d"
                  `(,key ,desc (lambda () (interactive) (+magit-diff-head-n ,n))))))
  (setq magit-auto-revert-mode nil))

(use-package vc
  :defer 0.2
  :ensure nil)

;;;###autoload
(defun +magit-diff-head-n (&optional arg)
  "Show the diff of HEAD and the previous `n' commits. Prompts for `n'
unless a nonzero and non-negative prefix is provided."
  (interactive "p")
  (magit-diff-range (format "HEAD~%d" arg)))

(use-package git-timemachine
  :commands (git-timemachine)
  :hook (git-timemachine-mode . +git-timemachine-setup)
  :bind
  (:map +leader-map
        ("gt" . #'git-timemachine)))

(defun +git-timemachine-setup ()
  "Set up motion mode with git-timemachine-quit override for q."
  (+motion-mode 1)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map +motion-mode-map)
    (define-key map "q" #'git-timemachine-quit)
    (setq-local minor-mode-overriding-map-alist
                (cons (cons '+motion-mode map)
                      minor-mode-overriding-map-alist))))

(use-package diff-hl
  :defer 0.5
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :bind
  (:map +leader-map
        ("gr" . #'diff-hl-revert-hunk))

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

(defun +magit-find-current-file-on-default-branch ()
  "View current file on the repository's default branch."
  (interactive)
  (let* ((file (file-relative-name
                (buffer-file-name)
                (magit-toplevel)))
         (default-ref
          (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))
         (branch
          (if (and default-ref
                   (string-match ".*/\\(.*\\)" default-ref))
              (match-string 1 default-ref)
            "main")))
    (magit-find-file branch file)))
