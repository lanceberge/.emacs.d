;;; -*- lexical-binding: t -*-
(use-package majutsu
  :ensure (:host github :repo "lanceberge/majutsu")
  :bind
  (:map +normal-mode-map
        ("j" . nil)
        ("ji" . #'+jj-init)
        ("jc" . #'+jj-git-clone)
        ("jN" . #'majutsu-new-dwim)
        ("jn" . #'+majutsu-new-hook)
        ("jr" . #'majutsu-rebase)
        ("jm" . #'+jj-describe)
        ("jd" . #'majutsu-diff-dwim)
        ("jl" . #'majutsu-log)
        ("ju" . #'majutsu-undo)
        ("jp" . #'majutsu-git-push)
        ("js" . #'+jj-squash)
        ("jf" . #'majutsu-git-fetch)
        ("ja" . #'majutsu-absorb)
        ("jbs" . #'majutsu-bookmark-set)
        ("jbt" . #'majutsu-bookmark-track)
        ("jba" . #'majutsu-bookmark-advance)
        ("jbn" . #'majutsu-bookmark-create)
        ("jw" . #'majutsu-workspace)
        ("jh" . #'majutsu-list-commits-for-file-dwim)
        ("jH" . #'majutsu-list-commits-for-file))
  (:map majutsu-log-mode-map
        ("P" . #'majutsu-git-push)
        ("m" . #'majutsu-describe))
  (:map majutsu-diff-mode-map
        ("P" . #'majutsu-git-push))
  :custom
  (majutsu-workspace-add-command #'project-find-file)
  (majutsu-workspace-add-dir #'+majutsu-workspace-add-dir)
  :init
  (require 'majutsu-git)
  :config
  ;; ignore immutable by default
  (with-eval-after-load 'majutsu-core
    (transient-define-argument majutsu-transient-arg-ignore-immutable ()
      :description "Ignore immutable"
      :class 'transient-switch
      :shortarg "-I"
      :argument "--ignore-immutable"
      :init-value (lambda (obj) (oset obj value "--ignore-immutable")))))

;; (use-package jujutsushi
;;   :ensure (jujutsushi
;;            :host sourcehut
;;            :repo "puercopop/jujutsushi"))

(defun +majutsu-workspace-add-dir ()
  "Default destination for `majutsu-workspace-add'.
Templates to `~/jj-workspaces/<repo-name>/`, where <repo-name> is the
basename of the current VC root."
  (let* ((root (or (vc-root-dir) default-directory))
         (repo-name (file-name-nondirectory
                     (directory-file-name (expand-file-name root)))))
    (expand-file-name (concat "jj-workspaces/" repo-name "/") "~")))


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

(defcustom +jj-git-clone-path "~/code"
  "Default parent directory for `+jj-git-clone'."
  :type 'directory
  :group '+jj)

;;;###autoload
(defun +jj-git-clone (url dir)
  "Run `jj git clone URL' into DIR, then `project-find-file' in the new repo.
URL defaults to the system clipboard contents."
  (interactive
   (list (string-trim
          (or (gui-get-selection 'CLIPBOARD)
              (current-kill 0 t)
              (user-error "Clipboard is empty")))
         (let ((vertico-preselect 'prompt))
           (read-directory-name "Clone into directory: "
                                (file-name-as-directory
                                 (expand-file-name +jj-git-clone-path))))))
  (let* ((repo-name (file-name-base
                     (replace-regexp-in-string "\\.git/?\\'" "" url)))
         (parent (expand-file-name dir))
         (target (expand-file-name repo-name parent)))
    (unless (file-directory-p parent)
      (make-directory parent t))
    (let ((default-directory parent))
      (message "%s"
               (shell-command-to-string
                (format "jj git clone %s %s"
                        (shell-quote-argument url)
                        (shell-quote-argument target)))))
    (let ((default-directory (file-name-as-directory target)))
      (call-interactively #'project-find-file))))
