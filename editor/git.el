;;; -*- lexical-binding: t -*-
(use-package transient :defer t)

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
        ([remap meow-line] . #'magit-discard))
  (:map +leader-map
        ("gs" . #'magit-status)
        ("gb" . #'magit-branch-checkout)
        ("gd" . #'magit-file-delete)
        ("gF" . #'magit-fetch)
        ("gnb" . #'magit-branch-and-checkout)
        ("gnf" . #'magit-commit-fixup)
        ("gi" . #'magit-init)
        ("gl" . #'magit-log)
        ("gf" . #'magit-find-file)
        ("gw" . #'magit-worktree)
        ("gc" . #'magit-show-commit))
  :config
  (setq magit-auto-revert-mode nil))

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
  :bind
  (:map +leader-map
        ("ml" . #'smerge-keep-upper)
        ("mo" . #'smerge-keep-lower)
        ("ma" . #'smerge-keep-all)
        ("mm" . #'smerge-ediff)
        ("mn" . #'+smerge-vc-next-conflict)
        ("mp" . #'smerge-prev))
  :config
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
             (goto-char prev-pos))))))))

(use-package diff-hl
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :defer 0.5
  :config
  (global-diff-hl-mode))
