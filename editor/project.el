;;; -*- lexical-binding: t -*-
(require 'subr-x)

(use-package project
  :demand t
  :commands
  (project-switch-project project-prompter project-root)
  :custom
  (project-switch-commands #'project-find-file)
  (project-mode-line t)
  :bind
  (:map +leader-map
        ("t" . #'+tmux-sessionizer-current-directory)
        ("'" . #'project-find-file)))


(use-package project-extensions
  :ensure (:type file :main "~/.emacs.d/packages/project-extensions.el")
  :demand t
  :bind
  (:map +leader-map
        ("SPC ;" . #'+project-switch-ripgrep)
        ("SPC '" . #'+project-switch)
        ("pk" . #'+project-kill-buffers)
        ("j" . #'+project-other-project)
        ("k" . #'+project-visit-last-buffer)
        ("l" . #'+project-other-buffer)
        ("pe" . #'flymake-show-project-diagnostics)
        ("pr" . #'+project-replace-regex)
        ("rk" . #'+project-reload-and-switch)
        ("rp" . #'+project-load-projects)
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
