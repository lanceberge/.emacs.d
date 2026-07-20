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
        ("jn" . #'+jj-new)
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

;; in development. This config unloads and reloads the whole package when evaluated
(use-package consult-jj
  :after consult
  :unless IS-WORK
  :load-path ("~/code/consult-jj"
              "~/code/consult-jj/extensions")
  :demand t
  :preface
  (dolist (feature '(consult-jj
                     consult-jj-jj
                     consult-jj-diff
                     consult-jj-hunk
                     consult-jj-commit))
    (when (featurep feature)
      (unload-feature feature t)))
  :bind
  (:map ctl-x-map
        ("v=" . #'consult-jj-modified-hunks)
        ("vf" . #'consult-jj-modified-files)
        ("vl" . #'consult-jj-log)))

(use-package consult-jj-embark
  :load-path ("~/code/consult-jj/extensions")
  :unless IS-WORK
  :after consult-jj
  :demand t

  :bind
  (:map consult-jj-modified-file-map
        ("c" . #'consult-jj-split)
        ("a" . #'consult-jj-squash))
  :config
  (when (featurep 'consult-jj-embark)
    (consult-jj-embark-mode -1)
    (unload-feature 'consult-jj-embark t))
  (require 'consult-jj-embark)
  (consult-jj-embark-mode 1))

(when IS-WORK
  (use-package consult-jj
    :ensure (:host github :repo "lanceberge/consult-gh" :files "extensions/*.el")
    :bind
    (:map ctl-x-map
          ("v=" . #'consult-jj-modified-hunks)
          ("vf" . #'consult-jj-modified-files)
          ("vl" . #'consult-jj-log))
    (:map consult-jj-modified-file-map
          ("c" . #'consult-jj-split)
          ("a" . #'consult-jj-squash))
    :config
    (require 'consult-jj-embark)
    (consult-jj-embark-mode 1)))

(use-package vc-jj)
