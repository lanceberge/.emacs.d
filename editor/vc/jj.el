;;; -*- lexical-binding: t -*-
(use-package majutsu
  :ensure (:host github :repo "0WD0/majutsu")
  :bind
  (:map +normal-mode
        ("j" . nil)
        ("ji" . #'+jj-init)
        ("jN" . #'majutsu-new-dwim)
        ("jn" . #'majutsu-new)
        ("jr" . #'majutsu-rebase)
        ("jt" . #'majutsu-bookmark-advance)
        ("jm" . #'+jj-describe)
        ("jd" . #'majutsu-diff)
        ("jl" . #'majutsu-log)
        ("ju" . #'majutsu-undo)
        ("jp" . #'majutsu-git-push)
        ("js" . #'+jj-squash)
        ("jf" . #'majutsu-git-fetch)
        ("jbs" . #'majutsu-bookmark-set)
        ("jbt" . #'majutsu-bookmark-track)
        ("jbn" . #'majutsu-bookmark-create))
  (:map majutsu-log-mode-map
        ("P" . #'majutsu-git-push)
        ("m" . #'majutsu-describe)))

;; (use-package jujutsushi
;;   :ensure (jujutsushi
;;            :host sourcehut
;;            :repo "puercopop/jujutsushi"))

(use-package vc-jj)

;;;###autoload
(defun +jj-describe (message)
  "Run `jj describe -m MESSAGE'."
  (interactive "sDescribe: ")
  (let ((output (shell-command-to-string
                 (format "jj describe -m %s" (shell-quote-argument message)))))
    (message "%s" (string-trim output))))

;;;###autoload
(defun +jj-squash ()
  (interactive)
  (let ((output (shell-command-to-string
                 "jj squash --ignore-immutable")))
    (message "%s" (string-trim output))))

;;;###autoload
(defun +jj-init ()
  (interactive)
  (let* ((project (project-current))
         (root (if project
                   (project-root project)
                 (read-directory-name "JJ init directory: ")))
         (default-directory root))
    (if (file-directory-p (expand-file-name ".jj" root))
        (message "Jujutsu repo already exists in %s" root)
      (message "%s"
               (shell-command-to-string
                "jj git init --colocate")))))
