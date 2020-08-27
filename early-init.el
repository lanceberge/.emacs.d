;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold most-positive-fixnum)

(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq emacs-load-start-time (current-time)
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      frame-inhibit-implied-resize t
      inhibit-default-init t
      site-run-file nil
      comp-deferred-compilation nil)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)


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
(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1
      straight-fix-org nil)

(straight-use-package 'use-package)

;; UI
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package display-line-numbers
  :straight (:type built-in)
  ;; :init
  ;; (setq-default display-line-numbers-width 5)
  ;; (setq-default display-line-numbers-widen t)
  ;; (setq-default display-line-numbers-width-start t)
  :config
  (global-display-line-numbers-mode)
  (when IS-LINUX
    (setq-default display-line-numbers-type 'visual)))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client-mode
                mode-line-modified
                mode-line-remote
                mode-line-frame-indentifcation
                " "
                mode-line-buffer-identification
                "  "
                (vc-mode vc-mode)
                " "
                mode-line-modes
                " "
                mode-line-misc-info
                mode-line-end-spaces))
