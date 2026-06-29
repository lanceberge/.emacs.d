;;; -*- lexical-binding: t -*-
(use-package project
  :demand t
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-use-entire-map t)
  (project-mode-line t)
  :bind
  (:map ctl-x-map
        ("d" . #'project-find-dir))
  (:map project-prefix-map
        ("C-g" . #'keyboard-quit)
        ("p" . #'+project-switch-project)
        ("o" . #'+project-other-buffer))
  (:map search-map
        ("f" . #'project-find-file)
        ("M-f" . #'project-root-find-file))
  (:map +leader-map
        ("k" . #'project-switch-project)))

(use-package project-compile
  :ensure (:type file :main "~/.emacs.d/lisp/project-compile.el")
  :hook
  (savehist-mode . +project-compile-save-hist-mode)
  :bind
  (:map project-prefix-map
        ("c" . #'+project-compile)))

(use-package project-extras
  :ensure (:type file :main "~/.emacs.d/lisp/project-extras.el")
  :bind
  (:map ctl-x-map
        ("pj" . #'+project-other-project-command)
        ("k" . #'+project-kill-buffer))
  (:map +leader-map
        ("k" . #'+project-switch-project)
        ("j" . #'+project-other-project-command)
        ("l" . #'+project-other-buffer)
        ("rk" . #'+project-reload-and-switch) ;; TODO replace w/ project reload prefix
        ("bl" . #'+project-other-special-buffer-dwim)))

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
