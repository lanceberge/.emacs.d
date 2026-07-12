;;; -*- lexical-binding: t -*-
;; increase GC threshold until startup (in Initial section under hooks)
(setq gc-cons-threshold most-positive-fixnum)

(defvar emacs-load-start-time (current-time))

(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-MAC (eq system-type 'darwin))

(setq package-enable-at-startup nil ; disable package.el at startup
      default-file-name-handler-alist file-name-handler-alist
      tramp-archive-enabled nil)

;; don't use custom
(setq custom-file nil)
(setq custom-delayed-init-variables '())

(if (fboundp 'native-comp-available-p)
    (if (native-comp-available-p)
        (setq comp-speed 2
              package-native-compile t
              native-comp-async-report-warnings-errors nil
              native-comp-deferred-compilation-deny-list nil
              native-comp-async-query-on-exit t)))

(setq warning-suppress-types '((defvaralias) (lexical-binding))
      warning-inhibit-types '((files missing-lexbind-cookie)))

(setq gnutls-verify-error t
      tls-checktrust gnutls-verify-error
      gnutls-min-prime-bits 3072)

(advice-add #'package--ensure-init-file :override #'ignore)

;; No unnecessary noise: toolbar, menu-bar, and scroll-bar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      frame-inhibit-implied-resize t
      inhibit-default-init t
      site-run-file nil)

(set-face-attribute 'default nil
                    :height 155
                    :weight 'normal
                    :width 'normal)

(advice-add #'x-apply-session-resources :override #'ignore)

(load (expand-file-name "early-init-local.el" user-emacs-directory) t)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; defer elisp compilation, great with native-comp branch
(setq comp-deferred-compilation t
      use-package-verbose t ; show which packages are being loaded on startup and when
      use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      byte-compile-warnings nil
      delete-by-moving-to-trash t
      read-process-output-max (* 4 1024 1024))

(setq enable-local-variables nil)

(use-package gruvbox-theme
  :ensure (:host github :repo "lanceberge/emacs-theme-gruvbox")
  :demand t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'gruvbox-dark-hard t))

(use-package display-line-numbers
  :ensure nil
  :demand t
  :hook
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'visual
        display-line-numbers-width-start t ; auto count number of lines to start numbers
                                        ; don't shrink line number width
        display-line-numbers-grow-only t))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                (:eval (if (fboundp '+modal--mode-line-indicator) (+modal--mode-line-indicator) ""))
                "   "
                mode-line-process
                (:eval (if (fboundp 'project-mode-line-format) (project-mode-line-format) ""))
                "   "
                mode-line-buffer-identification ; buffer name
                "  "
                ;; vc-mode
                (:eval (if (fboundp 'breadcrumb-imenu-crumbs) (breadcrumb-imenu-crumbs) ""))
                mode-line-format-right-align
                "      "))

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))

(when (>= emacs-major-version 29)
  (progn
    (setq use-short-answers t
          read-answer-short t)
    (add-to-list 'default-frame-alist '(undecorated-round . t))))
