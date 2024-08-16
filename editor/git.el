;;; -*- lexical-binding: t -*-
(use-package git-commit    :defer t)
(use-package transient     :defer t)
(use-package magit-section :defer t)

(use-package magit ; git client
  :defer 1.0
  :defer-incrementally
  (evil-collection magit-section dash f s with-editor git-commit package eieio lv transient)
  :custom
  (magit-save-repository-buffers nil)
  (magit-no-confirm '(stage-all-changes))
  :hook
  (git-commit-mode . evil-insert-state)
  :general
  (my-leader-def
    "gs"  #'(magit-status                :which-key "status")
    "gb"  #'(magit-branch-checkout       :which-key "checkout branch")
    "gd"  #'(magit-file-delete           :which-key "delete file")
    "gF"  #'(magit-fetch                 :which-key "fetch")
    "gnb" #'(magit-branch-and-checkout   :which-key "branch")
    "gnf" #'(magit-commit-fixup          :which-key "fixup commit")
    "gi"  #'(magit-init                  :which-key "init")
    "gl"  #'(magit-log                   :which-key "log")
    "gfc" #'(magit-show-commit           :which-key "show commit")
    "gfg" #'(magit-find-git-config-file  :which-key "git config file")
    "gc"  #'(+magit/stage-all-and-commit :which-key "stage all and commit"))
  :config
  (evil-collection-init 'magit)

  (setq evil-collection-magit-state 'normal
        evil-collection-magit-use-z-for-folds t
        magit-auto-revert-mode nil))

(use-package smerge-mode
  :straight (:type built-in)
  :general
  ('normal
   "]c" #'(smerge-next     :which-key "next conflicting hunk")
   "[c" #'(smerge-prev :which-key "previous conflicting hunk"))
  (my-localleader-def
    "ml" #'(smerge-keep-mine  :which-key "keep local changes")
    "mo" #'(smerge-keep-other :which-key "keep other changes")
    "ma" #'(smerge-keep-all   :which-key "keep all changes")
    "mm" #'(smerge-ediff      :which-key "merge"))
  :config
  (evil-collection-init 'ediff))

(use-package git-timemachine
  :general
  (my-leader-def
    "gt" #'(git-timemachine :which-key "git timemachine"))
  ('normal 'git-timemachine-mode-map
           "p" #'git-timemachine-show-previous-revision
           "n" #'git-timemachine-show-next-revision
           "q" #'git-timemachine-quit
           "t" #'git-timemachine-show-commit
           "r" #'write-file))

;;;###autoload
(defun +magit/stage-all-and-commit ()
  "stage all files and commit"
  (interactive)
  (save-buffer (current-buffer))
  (magit-stage-modified)
  (magit-commit-create)
  (magit-push))

(use-package diff-hl
  :defer 0.5
  :config
  (global-diff-hl-mode)
  :general
  ('normal
   "]h" #'diff-hl-next-hunk
   "[h" #'diff-hl-previous-hunk))
