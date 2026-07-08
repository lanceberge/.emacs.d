;;; -*- lexical-binding: t -*-
(use-package project
  :demand t
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-use-entire-map t)
  (project-mode-line t)
  (project-vc-ignores '("*.zip"))
  :bind
  (:map project-prefix-map
        ("C-g" . #'keyboard-quit)
        ("o" . #'keyboard-quit)
        ("s" . #'eat-project))
  (:map search-map
        ("f" . #'project-find-file)
        ("d" . #'project-find-dir)
        ("M-f" . #'project-root-find-file))
  (:map +leader-map
        ("k" . #'project-switch-project)))

(use-package project-compile
  :ensure (:type file :main "~/.emacs.d/lisp/project-compile.el" :files ("project-compile.el"))
  :hook
  (savehist-mode . +project-compile-save-hist-mode)
  :bind
  (:map project-prefix-map
        ("c" . #'+project-compile)))

;; TODO remove some of this
(use-package project-extras
  :ensure (:type file :main "~/.emacs.d/lisp/project-extras.el" :files ("project-extras.el"))
  :bind
  (:map +leader-map
        ("rp" . #'+project-reload-and-switch)))

;; update zoxide history -- cli tool that memoizes visited dirs
;;;###autoload
(defun +zoxide-add-current-directory ()
  (when-let ((dir (or (and buffer-file-name
                           (file-name-directory buffer-file-name))
                      (and (derived-mode-p 'dired-mode)
                           default-directory))))
    (call-process "zoxide" nil nil nil "add" dir)))

(when (executable-find "zoxide")
  (add-hook 'find-file-hook #'+zoxide-add-current-directory)
  (add-hook 'dired-mode-hook #'+zoxide-add-current-directory))
