;;; -*- lexical-binding: t -*-
(use-package consult-vc
  :ensure (:type file :main "~/.emacs.d/lisp/consult-vc.el" :files ("consult-vc.el"))
  :bind
  (:map +leader2-map
        ("gf" . #'+vc-modified-files)
        ("gh" . #'+vc-modified-hunks)))

(use-package magit
  :after modal
  :defer 1.0
  :custom
  (magit-save-repository-buffers nil)
  (magit-no-confirm '(stage-all-changes amend-published))
  (magit-diff-visit-prefer-worktree t)
  :hook
  (git-commit-mode . (lambda () (+insert-mode 1)))
  :bind
  (:map with-editor-mode-map
        ([remap delete-window] . #'with-editor-cancel)
        ([remap save-buffer] . #'with-editor-finish)
        ("C-x C-c" . #'with-editor-finish)
        ("C-c C-k" . #'with-editor-cancel))
  (:map magit-blob-mode-map
        ("q" . #'quit-window)
        ([remap save-buffer] . #'quit-window)
        ([remap +elisp-validate-balanced-parens] . #'quit-window))
  (:map magit-diff-section-map
        ("C-j" . #'magit-section-forward))
  (:map magit-status-mode-map
        ("C-j" . #'magit-section-forward)
        ("C-k" . #'magit-section-backward)
        ("M-j" . #'magit-jump-to-staged)
        ("M-k" . #'magit-jump-to-unstaged)
        ("q" . #'magit-commit)
        ("x" . #'magit-discard)
        ("V" . #'set-mark-command))
  (:map +leader-map
        ("gb" . #'magit-branch-or-checkout)
        ("gc" . #'magit-branch-spinoff)
        ("gi" . #'magit-init)
        ("gl" . #'magit-log-head)
        ("SPC gl" . #'magit-log)
        ("gf" . #'magit-find-file)
        ("gm" . #'+magit-find-current-file-on-default-branch)
        ("gw" . #'magit-worktree)
        ("gh" . #'+magit-diff-head-n)
        ("gs" . #'+magit-diff-source)
        ("ge" . #'+magit-ediff-source))
  :config
  (+modal-bind +motion-mode-map magit-status-mode-hook
               ("c" . #'magit-commit))

  (cl-loop for n from 1 to 9
           do (let ((key (number-to-string n))
                    (desc (format "Diff HEAD~%d" n)))
                (transient-append-suffix 'magit-diff "d"
                  `(,key ,desc (lambda () (interactive) (+magit-diff-head-n ,n))))))
  (setq magit-auto-revert-mode nil))

(use-package vc
  :defer 0.2
  :ensure nil
  :custom
  (vc-git-print-log-follow t)
  (vc-git-diff-switches '("--histogram"))
  :bind
  (:map ctl-x-map
        ("vr" . #'vc-revert)))

(defun +magit-source-branch ()
  "Return the remote branch the current branch was branched off of.
Finds the nearest remote branch by walking the commit history."
  (let* ((current (magit-get-current-branch))
         (decorations (magit-git-lines
                       "log" "--simplify-by-decoration" "--format=%D" "HEAD"))
         (source
          (cl-loop for line in (cdr decorations) ;; skip first (HEAD)
                   thereis
                   (cl-loop for ref in (split-string line ", " t)
                            when (and (string-prefix-p "origin/" ref)
                                      (not (equal ref (concat "origin/" current))))
                            return ref))))
    (unless source
      (error "Could not determine source branch"))
    source))

;;;###autoload
(defun +magit-diff-source ()
  "Diff HEAD against the remote branch the current branch was forked from."
  (interactive)
  (let ((source (+magit-source-branch)))
    (message "Diffing against %s" source)
    (magit-diff-range source)
    (delete-other-windows)))

;;;###autoload
(defun +magit-ediff-source ()
  "Ediff a modified file against the source branch.
Prompts for which file to compare from the list of files changed
relative to the source branch."
  (interactive)
  (let* ((source (+magit-source-branch))
         (files (magit-git-lines "diff" "--name-only" source))
         (file (completing-read
                (format "Ediff against %s: " source)
                files nil t)))
    (magit-ediff-compare source nil file file)))

;;;###autoload
(defun +magit-diff-head-n (&optional arg)
  "Show the diff of HEAD and the previous `n' commits. Prompts for `n'
unless a nonzero and non-negative prefix is provided."
  (interactive "p")
  (magit-diff-range (format "HEAD~%d" arg)))

(use-package git-timemachine
  :after modal
  :commands (git-timemachine)
  :hook (git-timemachine-mode . +git-timemachine-setup)
  :bind
  (:map +leader-map
        ("gt" . #'git-timemachine))
  :config
  (+modal-bind +motion-mode-map git-timemachine-mode-hook
               ("q" . #'git-timemachine-quit)))

(defun +git-timemachine-setup ()
  "Set up motion mode for git-timemachine."
  (+motion-mode 1))

(defvar +majutsu-post-new-hook nil
  "Hook run after `majutsu-new' finishes.")

(declare-function majutsu-toplevel "majutsu-jj")
(declare-function diff-hl-update "diff-hl")
(defvar diff-hl-disable-on-remote)
(defvar diff-hl-mode)

(defun majutsu-diff-hl-post-refresh ()
  "Refresh diff-hl in file buffers under the current jj repo.
jj-aware analogue of `diff-hl-magit-post-refresh' suitable for
`majutsu-post-refresh-hook' and similar majutsu hooks."
  (unless (and (bound-and-true-p diff-hl-disable-on-remote)
               (file-remote-p default-directory))
    (when-let* ((topdir (majutsu-toplevel)))
      (dolist (buf (buffer-list))
        (when (and (buffer-local-value 'diff-hl-mode buf)
                   (not (buffer-modified-p buf))
                   (buffer-file-name buf)
                   (file-in-directory-p (buffer-file-name buf) topdir)
                   (file-exists-p (buffer-file-name buf)))
          (with-current-buffer buf
            (let* ((file buffer-file-name)
                   (backend (vc-backend file)))
              (when backend
                (vc-state-refresh file backend)
                (diff-hl-update)))))))))

(use-package diff-hl
  :defer 0.5
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (majutsu-post-refresh . majutsu-diff-hl-post-refresh)
  (+majutsu-post-new-hook . majutsu-diff-hl-post-refresh)
  :bind
  (:map +normal-mode-map
        ("[h" . #'diff-hl-previous-hunk)
        ("]h" . #'diff-hl-previous-hunk))
  (:map ctl-x-map
        ("vs" . #'diff-hl-show-hunk)
        ("v[" . #'diff-hl-previous-hunk)
        ("v]" . #'diff-hl-next-hunk)
        ("vu" . #'diff-hl-revert-hunk))
  :config
  (global-diff-hl-mode))

(use-package git-link
  :custom
  (git-link-use-commit t)
  :bind
  (:map +leader2-map
        ("gl" . #'git-link)))

;;;###autoload
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
