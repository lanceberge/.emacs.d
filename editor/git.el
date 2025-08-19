;;; -*- lexical-binding: t -*-
(use-package transient :defer t)

(use-package magit
  :defer 1.0
  :defer-incrementally
  (magit-section dash f s with-editor git-commit package eieio lv transient)
  :custom
  (magit-save-repository-buffers nil)
  (magit-no-confirm '(stage-all-changes amend-published))
  :general
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
  :general
  ('meow-normal-state-keymap 'magit-mode-map
                             ";" (general-simulate-key "RET"))
  :config
  (setq magit-auto-revert-mode nil))

(use-package smerge-mode
  :ensure nil
  :general
  ;; TODO
  ;; ('meow-normal-state-keymap
  ;;  "]c" #'(smerge-vc-next-conflict :which-key "next conflicting hunk")
  ;;  "[c" #'(smerge-prev :which-key "previous conflicting hunk"))
  ;; (my-localleader-def
  ;;   "ml" #'(smerge-keep-upper :which-key "keep local changes")
  ;;   "mo" #'(smerge-keep-lower :which-key "keep other changes")
  ;;   "ma" #'(smerge-keep-all :which-key "keep all changes")
  ;;   "mm" #'(smerge-ediff :which-key "merge"))
  )

(use-package git-timemachine
  :general
  (my-leader-def
    "gt" #'(git-timemachine :which-key "git timemachine"))
  ('meow-normal-state-keymap 'git-timemachine-mode-map
                             "p" #'git-timemachine-show-previous-revision
                             "n" #'git-timemachine-show-next-revision
                             "q" #'git-timemachine-quit
                             "t" #'git-timemachine-show-commit
                             "r" #'write-file))

;;;###autoload
;; TODO
(defun +magit/stage-all-and-commit ()
  "stage all files and commit"
  (interactive)
  (save-buffer (current-buffer))
  (magit-stage-modified)
  (magit-commit-create)
  (magit-push))

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
