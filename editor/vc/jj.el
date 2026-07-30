;;; -*- lexical-binding: t -*-

(use-package jj-extras
  :ensure (:type file :main "~/.emacs.d/lisp/jj-extras.el" :files ("jj-extras.el"))
  :init
  (setq majutsu-workspace-add-command #'+jj-workspace-after-add
        majutsu-workspace-add-dir #'+jj-workspace-add-dir
        majutsu-workspace-forget-command #'+majutsu-forget-command)
  :bind
  (:map +normal-mode-map
        ("ji" . #'+jj-init)
        ("jc" . #'+jj-git-clone)
        ;; ("jn" . #'+jj-new)
        ("jm" . #'+jj-describe)
        ("js" . #'+jj-squash)))

;;;###autoload
(defun +majutsu-forget-command (directory)
  (let ((project-root (file-name-as-directory
                       (expand-file-name directory))))
    (when-let* (((and (fboundp 'project-current)
                      (fboundp 'project-kill-buffers)))
                (project (project-current nil project-root)))
      (project-kill-buffers t project))
    (when (fboundp 'project-forget-project)
      (project-forget-project project-root))
    (when (file-directory-p project-root)
      (delete-directory project-root t))))

(use-package majutsu
  :ensure (:host github :repo "lanceberge/majutsu")
  :init
  (autoload 'majutsu-git-fetch "majutsu-git" nil t)
  (autoload 'majutsu-git-push "majutsu-git" nil t)
  :bind
  (:map +normal-mode-map
        ("j SPC c" . #'majutsu-commit)
        ;; ("jN" . #'majutsu-new-dwim)
        ;; ("jr" . #'majutsu-rebase)
        ;; ("jd" . #'majutsu-diff-dwim)
        ("jE" . #'+ediff-conflicts)
        ;; ("jl" . #'majutsu-log)
        ;; ("ju" . #'majutsu-undo)
        ;; ("jp" . #'majutsu-git-push)
        ;; ("jf" . #'majutsu-git-fetch)
        ("ja" . #'majutsu-absorb)
        ("jbs" . #'majutsu-bookmark-set)
        ("jbt" . #'majutsu-bookmark-track)
        ;; ("jba" . #'majutsu-bookmark-advance)
        ("jbn" . #'majutsu-bookmark-create)
        ;; ("jw" . #'majutsu-workspace)
        ("jh" . #'majutsu-list-commits-for-file-dwim)
        ("j SPC h" . #'majutsu-list-commits-for-file)) ;; TODO should be an embark file command
  (:map majutsu-log-mode-map
        ("P" . #'majutsu-git-push)
        ("m" . #'majutsu-describe))
  (:map majutsu-diff-mode-map
        ("P" . #'majutsu-git-push)))

;; in development. This config unloads and reloads the whole package when evaluated
(use-package consult-jj
  :after consult
  :load-path ("~/code/consult-jj"
              "~/code/consult-jj/extensions")
  :ensure `(,@(when IS-WORK
                '(:host github
                        :repo "~/jj-workspaces/consult-jj/marginalia-annotations/"
                        :files "extensions/*.el")))
  :demand t
  :preface
  (unless IS-WORK
    (dolist (feature '(consult-jj
                       consult-jj-jj
                       consult-jj-diff
                       consult-jj-hunk
                       consult-jj-marginalia
                       consult-jj-embark
                       consult-jj-diff-hl
                       consult-jj-tag
                       consult-jj-workspace
                       consult-jj-commit))
      (when (featurep feature)
        (unload-feature feature t))))
  :bind
  (:map +normal-mode-map
        ("jl" . #'consult-jj-log)
        ("jt" . #'consult-jj-tag)
        ("jo" . #'consult-jj-op-log)
        ("ju" . #'consult-jj-undo)
        ("jn" . #'consult-jj-new-here)
        ("jd" . #'consult-jj-modified-files)
        ("jf" . #'consult-jj-git-fetch)
        ("jro" . #'consult-jj-rebase-onto)
        ("jbs" . #'consult-jj-bookmark-set)
        ("jbc" . #'consult-jj-bookmark-create)
        ("jbl" . #'consult-jj-bookmark)
        ("jwl" . #'consult-jj-workspace-list)
        ("jwa" . #'consult-jj-workspace-add)
        ("jwu" . #'consult-jj-workspace-update-stale)
        ("jba" . #'consult-jj-bookmark-advance)
        ("jrb" . #'consult-jj-rebase-before)
        ("jra" . #'consult-jj-rebase-after)
        ("jp" . #'consult-jj-git-push))
  (:map ctl-x-map
        ("v=" . #'consult-jj-modified-hunks)
        ("vf" . #'consult-jj-modified-files)
        ("vl" . #'consult-jj-log))
  (:map consult-jj-commit-map
        ("a" . #'consult-jj-commit-squash)
        ("m" . #'consult-jj-commit-describe)
        ("A" . #'consult-jj-commit-abandon))
  (:map consult-jj-modified-file-map
        ("c" . #'consult-jj-split)
        ("a" . #'consult-jj-squash))
  :config
  (require 'consult-jj-embark)
  (consult-jj-embark-mode 1)
  (require 'consult-jj-diff-hl)
  (consult-jj-diff-hl-mode 1)
  (require 'consult-jj-marginalia)
  (consult-jj-marginalia-mode 1)
  (consult-jj-commit-two-line-mode 1))

(use-package vc-jj
  :after project
  :demand t
  :config
  (require 'project-jj))
