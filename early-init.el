;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
		    :height 110
		    :weight 'normal
 		    :width 'normal)

(setq frame-inhibit-implied-resize t)

(eval-when-compile
  (require 'package))

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/"))
      load-prefer-newer t)
(package-initialize)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))
