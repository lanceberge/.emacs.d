;;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq emacs-load-start-time (current-time))

(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
		    :height 110
		    :weight 'normal
		    :width 'normal)

(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)

;; Boostrap straight.el
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
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))
