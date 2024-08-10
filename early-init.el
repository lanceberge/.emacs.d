;;; -*- lexical-binding: t -*-
;; increase GC threshold until startup (in Initial section under hooks)
(setq gc-cons-threshold most-positive-fixnum)

(defconst IS-LINUX   (eq system-type   'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-MAC     (eq system-type   'darwin))

(setq package-enable-at-startup nil) ; disable package.el at startup
(advice-add #'package--ensure-init-file :override #'ignore)

;; No unnecessary noise: toolbar, menu-bar, and scroll-bar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq emacs-load-start-time (current-time)
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      frame-inhibit-implied-resize t
      inhibit-default-init t
      site-run-file nil)

(set-face-attribute 'default nil ; font
                    :family "DejaVu Sans Mono"
                    :height 180
                    :weight 'normal
                    :width 'normal)

(advice-add #'x-apply-session-resources :override #'ignore)

;; Bootstrap straight.el - my package manager
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package uses straight.el
(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1
      straight-fix-org nil
      comp-deferred-compilation t) ; defer elisp compilation, great with native-comp branch

(straight-use-package 'use-package)
(setq use-package-verbose t) ; show which packages are being loaded on startup and when

(defconst bg-color-light "#32302f"
  "gruvbox background color")

(defconst bg-color "#282828"
  "gruvbox background color")

(use-package autothemer :defer t)

(use-package gruvbox-theme ; theme
  :config
  (load-theme 'gruvbox t))

(use-package display-line-numbers
  :straight (:type built-in)
  :custom-face
  (line-number              ((t (:background ,bg-color))))
  (line-number-current-line ((t (:background ,bg-color))))
  :config
  (unless IS-WINDOWS
    (setq-default display-line-numbers-type 'visual
                  display-line-numbers-width-start t ; auto count number of lines to start numbers
                  display-line-numbers-grow-only t ; don't shrink line number width
                  ))

  (global-display-line-numbers-mode))

;; Minimalistic mode-line
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client-mode
                mode-line-modified
                mode-line-remote
                mode-line-frame-indentifcation
                " "
                mode-line-buffer-identification ; buffer name
                "  "
                vc-mode                         ; show git branch
                " "
                mode-line-modes                 ; show current mode
                " "
                mode-line-misc-info
                mode-line-end-spaces))

;; Mode-line faces
(custom-set-faces `(mode-line           ((t (:background ,bg-color-light :foreground "#928374"))))
                  `(mode-line-inactive  ((t (:background ,bg-color)))))

(set-face-foreground 'vertical-border bg-color)

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))

(use-package minions ; hide all minor modes in modeline
  :demand t
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '(" " . ""))
  :config
  (minions-mode 1))
