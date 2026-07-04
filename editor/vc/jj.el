;;; -*- lexical-binding: t -*-

(use-package jj-extras
  :ensure (:type file :main "~/.emacs.d/lisp/jj-extras.el" :files ("jj-extras.el"))
  :init
  (setq majutsu-workspace-add-command #'+jj-workspace-after-add
        majutsu-workspace-add-dir #'+jj-workspace-add-dir)
  :bind
  (:map +normal-mode-map
        ("ji" . #'+jj-init)
        ("jc" . #'+jj-git-clone)
        ("jn" . #'+jj-new)
        ("jm" . #'+jj-describe)
        ("js" . #'+jj-squash)))

(use-package majutsu
  :ensure (:host github :repo "lanceberge/majutsu")
  :bind
  (:map +normal-mode-map
        ("j SPC c" . #'majutsu-commit)
        ("jN" . #'majutsu-new-dwim)
        ("jr" . #'majutsu-rebase)
        ("jd" . #'majutsu-diff-dwim)
        ("jE" . #'+ediff-conflicts)
        ("jl" . #'majutsu-log)
        ("ju" . #'majutsu-undo)
        ("jp" . #'majutsu-git-push)
        ("jf" . #'majutsu-git-fetch)
        ("ja" . #'majutsu-absorb)
        ("jbs" . #'majutsu-bookmark-set)
        ("jbt" . #'majutsu-bookmark-track)
        ("jba" . #'majutsu-bookmark-advance)
        ("jbn" . #'majutsu-bookmark-create)
        ("jw" . #'majutsu-workspace)
        ("jh" . #'majutsu-list-commits-for-file-dwim)
        ("j SPC h" . #'majutsu-list-commits-for-file)) ;; TODO should be an embark file command
  (:map majutsu-log-mode-map
        ("P" . #'majutsu-git-push)
        ("m" . #'majutsu-describe))
  (:map majutsu-diff-mode-map
        ("P" . #'majutsu-git-push)))

(use-package vc-jj)
