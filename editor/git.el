;;; -*- lexical-binding: t -*-
(use-package transient
  :bind
  (:map transient-base-map
        ("M-p" . #'transient-reset))
  :config
  (transient-bind-q-to-quit))

(use-package magit
  :defer 1.0
  :defer-incrementally
  (magit-section dash f s with-editor git-commit package eieio lv transient)
  :custom
  (magit-save-repository-buffers nil)
  (magit-no-confirm '(stage-all-changes amend-published))
  :hook
  (git-commit-mode . meow-insert-mode)
  :bind
  (:map git-commit-mode
        ([remap delete-window] . #'with-editor-cancel))
  (:map with-editor-mode-map
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
        ("gb" . #'magit-branch-checkout)
        ("gd" . #'magit-file-delete)
        ("gF" . #'magit-fetch)
        ("gnb" . #'magit-branch-and-checkout)
        ("gnf" . #'magit-commit-fixup)
        ("gi" . #'magit-init)
        ("gl" . #'magit-log-head)
        ("g SPC l" . #'magit-log)
        ("gf" . #'magit-find-file)
        ("gw" . #'magit-worktree)
        ("gh" . #'+magit-diff-head-n)
        ("gc" . #'magit-show-commit))
  :config
  (cl-loop for n from 1 to 9
           do (let ((key (number-to-string n))
                    (desc (format "Diff HEAD~%d" n)))
                (transient-append-suffix 'magit-diff "d"
                  `(,key ,desc (lambda () (interactive) (+magit-diff-head-n ,n))))))
  (setq magit-auto-revert-mode nil))

(defun +magit-diff-head-n (n)
  "Show the diff of HEAD and the previous `n' commits. Prompts for `n'
unless a nonzero and non-negative prefix is provided."
  (interactive
   (list
    (if (and current-prefix-arg
             (> (prefix-numeric-value current-prefix-arg) 0))
        (prefix-numeric-value current-prefix-arg)
      (read-number "Enter number of commits: " 1))))
  (magit-diff-range (format "HEAD~%d" n)))

(use-package git-timemachine
  :commands (git-timemachine)
  :hook (git-timemachine-mode . +meow-motion-mode)
  :bind
  (:map +leader-map
        ("gt" . #'git-timemachine))
  (:map git-timemachine-mode-map
        ([remap meow-quit] . #'git-timemachine-quit)))

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode)
  :bind
  (:map +leader-map
        ("ml" . #'smerge-keep-upper)
        ("mo" . #'smerge-keep-lower)
        ("ma" . #'smerge-keep-all)
        ("mm" . #'smerge-ediff)
        ("mn" . #'+smerge-vc-next-conflict)
        ("mp" . #'smerge-prev)))

;;;###autoload
(defun +smerge-vc-next-conflict ()
  (interactive)
  (condition-case nil
      (smerge-next)
    (error
     (if (and (buffer-modified-p) buffer-file-name)
         (save-buffer))
     (vc-find-conflicted-file)
     (unless (looking-at "^<<<<<<<")
       (let ((prev-pos (point)))
         (goto-char (point-min))
         (unless (ignore-errors (not (smerge-next)))
           (goto-char prev-pos)))))))

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
