;;; -*- lexical-binding: t -*-
(use-package majutsu
  :ensure (:host github :repo "0WD0/majutsu")
  :bind
  (:map +normal-mode
        ("j" . nil)
        ("ji" . #'+jj-init)
        ("jN" . #'majutsu-new-dwim)
        ("jn" . #'+majutsu-new-hook)
        ("jr" . #'majutsu-rebase)
        ("jm" . #'+jj-describe)
        ("jd" . #'majutsu-diff)
        ("jl" . #'majutsu-log)
        ("ju" . #'majutsu-undo)
        ("jp" . #'majutsu-git-push)
        ("js" . #'+jj-squash)
        ("jf" . #'majutsu-git-fetch)
        ("ja" . #'majutsu-absorb)
        ("jbs" . #'majutsu-bookmark-set)
        ("jbt" . #'majutsu-bookmark-track)
        ("jba" . #'majutsu-bookmark-advance)
        ("jbn" . #'majutsu-bookmark-create))
  (:map majutsu-log-mode-map
        ("P" . #'majutsu-git-push)
        ("m" . #'majutsu-describe))
  :init
  (require 'majutsu-git))

;; (use-package jujutsushi
;;   :ensure (jujutsushi
;;            :host sourcehut
;;            :repo "puercopop/jujutsushi"))

(use-package vc-jj)

(defvar +jj-post-squash-hook nil
  "Hook run after `+jj-squash' finishes.")

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
    (message "%s" (string-trim output))
    (run-hooks '+jj-post-squash-hook)))

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

;;;###autoload
(defun +majutsu-new ()
  (interactive)
  (call-interactively #'majutsu-new)
  (run-hooks #'+majutsu-post-new-hook))
