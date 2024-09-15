;;; -*- lexical-binding: t -*-
;; increase GC threshold until startup (in Initial section under hooks)
(setq gc-cons-threshold most-positive-fixnum)

(defvar emacs-load-start-time (current-time))

(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-MAC (eq system-type 'darwin))

(setq package-enable-at-startup nil) ; disable package.el at startup
(if (fboundp 'native-comp-available-p)
    (if (native-comp-available-p)
        (setq comp-speed 2
              package-native-compile t
              native-comp-async-report-warnings-errors nil
              native-comp-deferred-compilation-deny-list nil)))

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

(set-face-attribute 'default nil ; font
                    :family "DejaVu Sans Mono"
                    :height 180
                    :weight 'normal
                    :width 'normal)

(advice-add #'x-apply-session-resources :override #'ignore)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; defer elisp compilation, great with native-comp branch
(setq comp-deferred-compilation t)

(setq use-package-verbose t ; show which packages are being loaded on startup and when
      use-package-always-ensure t)

(defconst bg-color-light "#32302f"
  "gruvbox background color")

(defconst bg-color "#282828"
  "gruvbox background color")

(use-package autothemer :defer t)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package display-line-numbers
  :ensure nil
  :custom-face
  (line-number ((t (:background ,bg-color))))
  (line-number-current-line ((t (:background ,bg-color))))
  :config
  (unless IS-WINDOWS
    (setq-default display-line-numbers-type 'visual
                  display-line-numbers-width-start t ; auto count number of lines to start numbers
                  display-line-numbers-grow-only t)) ; don't shrink line number width

  (global-display-line-numbers-mode))

;; https://github.com/progfolio/elpaca/issues/216
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(elpaca `(seq :build ,(+elpaca-seq-build-steps)))

(use-package keycast
  :init
  (keycast-mode-line-mode)
  :custom-face
  (keycast-key ((t (:box nil :background ,bg-color-light :foreground "#b8bb26")))))

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
                vc-mode ; show git branch
                " "
                mode-line-modes ; show current mode
                " "
                mode-line-misc-info
                mode-line-end-spaces
                keycast-mode-line))

;; Mode-line faces
(custom-set-faces `(mode-line ((t (:background ,bg-color-light :foreground "#928374"))))
                  `(mode-line-inactive ((t (:background ,bg-color)))))

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

(setq byte-compile-warnings nil
      read-process-output-max (* 1024 1024))

(if (>= emacs-major-version 29)
    (progn
      (setopt use-short-answers t)
      (add-to-list 'default-frame-alist '(undecorated-round . t))))
