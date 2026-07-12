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
  :config
  (bind-key "a" +llm-map project-prefix-map))

;; save project-local compile histories
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
