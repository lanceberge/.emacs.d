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
  :general
  ('with-editor-mode-map
   [remap save-buffer] #'with-editor-finish)
  (my-leader-def
    "gs" #'(magit-status :which-key "status")
    "gb" #'(magit-branch-checkout :which-key "checkout branch")
    "gd" #'(magit-file-delete :which-key "delete file")
    "gF" #'(magit-fetch :which-key "fetch")
    "gnb" #'(magit-branch-and-checkout :which-key "branch")
    "gnf" #'(magit-commit-fixup :which-key "fixup commit")
    "gi" #'(magit-init :which-key "init")
    "gl" #'(magit-log :which-key "log")
    "gf" #'(magit-find-file :which-key "find file")
    "gw" #'(magit-worktree :which-key "worktree")
    "gc" #'(magit-show-commit :which-key "show commit"))
  ('magit-diff-section-map
   "C-j" #'magit-section-forward)
  ('magit-status-mode-map
   "C-j" #'magit-section-forward
   "C-k" #'magit-section-backward
   "M-j" #'magit-jump-to-staged
   "M-k" #'magit-jump-to-unstaged
   "q" #'magit-commit
   "p" #'magit-push
   "x" #'magit-discard
   [remap meow-line] #'magit-discard)
  :config
  (setq magit-auto-revert-mode nil))

(use-package git-timemachine
  :commands (git-timemachine)
  :hook (git-timemachine-mode . +meow-motion-mode)
  :general
  (my-leader-def
    "gt" #'git-timemachine)
  (git-timemachine-mode-map
   [remap meow-quit] #'git-timemachine-quit))

(use-package smerge-mode
  :ensure nil
  :general
  (my-leader-def
    "ml" #'(smerge-keep-upper :which-key "keep local changes")
    "mo" #'(smerge-keep-lower :which-key "keep other changes")
    "ma" #'(smerge-keep-all :which-key "keep all changes")
    "mm" #'(smerge-ediff :which-key "merge")
    "mn" (defun +smerge-vc-next-conflict ()
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
    "mp" #'smerge-prev))

(use-package diff-hl
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :defer 0.5
  :config
  (global-diff-hl-mode)
  ;; :general
  ;; ('meow-normal-state-keymap
  ;;  "]h" #'diff-hl-next-hunk
  ;;  "[h" #'diff-hl-previous-hunk)
  )
