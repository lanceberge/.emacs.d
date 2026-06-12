;;; -*- lexical-binding: t -*-
(if (version< emacs-version "27.1")
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(setopt default-file-name-handler-alist file-name-handler-alist
        ;; unset file-name-handler-alist until its set again in Hooks heading (improve startup time)
        file-name-handler-alist nil

        ;; raise garbage collection threshold until its set again in Hooks heading
        gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6

        load-prefer-newer noninteractive
        locale-coding-system 'utf-8
        message-log-max 5000 ; longer number of max messages
        ring-bell-function 'ignore

        ;; startup.el settings
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name

        ;; inhibit-default-init t
        initial-scratch-message nil ; empty scratch file
        initial-major-mode 'fundamental-mode ; start in an empty mode

        find-file-visit-truename t ; auto go to symlinks
        vc-follow-symlinks t
        confirm-kill-emacs 'y-or-n-p
        kill-buffer-query-functions nil
        inhibit-compacting-font-caches t ; inhibit font compacting
        highlight-nonselected-windows t
        ffap-machine-p-known 'reject ; don't ping things that look like domain names
        bidi-inhibit-bpa t
        fast-but-imprecise-scrolling t ; faster scrolling over unfontified regions
        redisplay-skip-fontification-on-input t

        ;; Scrolling
        scroll-conservatively 1000
        scroll-margin 4
        scroll-preserve-screen-position t
        scroll-step 1

        ;; General
        apropos-do-all t ; more extensive apropos searches
        kill-whole-line t
        save-interprogram-paste-before-kill t
        global-mark-ring-max 64

        use-file-ddialog nil
        use-dialog-box nil
        pop-up-frames nil

        debugger-stack-frame-as-list nil

        history-delete-duplicates t

        ;; Disable bidirectional text rendering for performance
        bidi-display-reordering 'left-to-right
        bidi-paragraph-direction 'left-to-right
        cursor-in-non-selected-windows nil

        ;; Security
        enable-local-eval nil
        epg-pinentry-mode 'loopback
        auth-sources '("~/.authinfo.gpg")

        ;; Editing defaults
        uniquify-buffer-name-style 'forward
        sentence-end-double-space nil
        comment-multi-line t
        comment-empty-lines t
        mouse-yank-at-point t
        tabify-regexp (rx line-start (zero-or-more ?\t) ?\s (one-or-more blank))
        fill-nobreak-invisible t

        ;; Paren
        blink-matching-paren nil

        ;; Help / find-library
        find-library-include-other-files nil
        help-enable-completion-autoload nil
        help-enable-autoload nil
        help-enable-symbol-autoload nil
        help-window-select t

        ;; Misc UX
        delete-pair-blink-delay 0.03
        global-text-scale-adjust-resizes-frames nil
        diff-font-lock-prettify t
        set-mark-command-repeat-pop t

        ;; Dabbrev
        dabbrev-upcase-means-case-search t
        dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode
                                                    tags-table-mode pdf-view-mode)
        dabbrev-ignored-buffer-regexps '("\\` "
                                         "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")

        ;; Compilation
        compilation-ask-about-save nil
        compilation-always-kill t
        compilation-max-output-line-length 2048
        compilation-scroll-output 'first-error

        ;; Comint
        ansi-color-for-comint-mode t
        comint-prompt-read-only t
        comint-buffer-maximum-size 4096

        ;; Tramp
        tramp-verbose 1
        remote-file-name-inhibit-cache 50
        remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t)

(when IS-MAC
  (dolist (path '("/opt/homebrew/opt/llvm/bin/"
                  "~/go/bin"
                  "/Users/lance/.config/herd-lite/bin/"
                  "/opt/homebrew/bin/"
                  "/opt/homebrew/opt/openjdk/bin/"
                  "~/.asdf/shims/"
                  "~/.config/composer/vendor/bin/"))
    (add-to-list 'exec-path path))
  (setenv "PATH" (mapconcat 'identity exec-path path-separator)))

(setq-default
 tab-width 4
 require-final-newline t
 indent-tabs-mode nil ; tabs are converted to spaces
 word-wrap t
 truncate-lines t
 fill-column 80)

(blink-cursor-mode -1)

(dolist (cmd '(narrow-to-region narrow-to-page upcase-region downcase-region
                                erase-buffer dired-find-alternate-file
                                set-goal-column list-timers list-threads
                                scroll-left))
  (put cmd 'disabled nil))

(advice-add #'tty-run-terminal-initialization :override #'ignore)

;; Get rid of For information about GNU Emacs message
(advice-add #'display-startup-echo-area-message :override #'ignore)

(fset 'yes-or-no-p 'y-or-n-p) ; y or n prompt, not yes or no

(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super
        abbreviated-home-dir "\\ `'"))

(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

(unless IS-MAC
  (setq command-line-ns-option-alist nil))

(unless IS-LINUX
  (setq command-line-x-option-alist nil))

(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

(defvar +leader-map (make-sparse-keymap))
(defvar +leader2-map (make-sparse-keymap))
(defvar +leader3-map (make-sparse-keymap))
(defvar +x-map (make-sparse-keymap))

(defvar mark-forward-keymap (make-sparse-keymap))
(defvar mark-backward-keymap (make-sparse-keymap))

(defvar +normal-mode-map (make-sparse-keymap))
(defvar +insert-mode-map (make-sparse-keymap))
(defvar +motion-mode-map (make-sparse-keymap))
(defvar +sexp-mode-map (make-keymap))

(add-hook 'after-init-hook
          (lambda ()
            "show the startup time"
            (when (require 'time-date nil t)
              (message "Emacs init time: %.2f seconds."
                       (time-to-seconds (time-since emacs-load-start-time))))))

(add-hook 'emacs-startup-hook
          (lambda ()
            "raise the garbage collection threshold to defer garbage collection
           and unset file-name-handler-alist"
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;; Raise gc threshold while minibuffer is active
(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (run-at-time
               1 nil (lambda () (setq gc-cons-threshold 17772160)))))

(use-package compat
  :ensure t)

(use-package gcmh ; Garbage collect in idle time
  :defer 2.0
  :commands gcmh-idle-garbage-collect
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 17772160)
  :config
  (gcmh-mode)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))

(when (and IS-LINUX (>= emacs-major-version 29))
  (set-frame-parameter nil 'undecorated t))

;; Start the emacsclient on init
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (condition-case err
        (server-start)
      (error
       (message "server-start failed: %s; deleting stale server and retrying"
                (error-message-string err))
       (server-force-delete)
       (server-start)))))

(use-package modal
  :ensure (:type file :main "~/.emacs.d/packages/modal.el")
  :demand t)

(let ((dirs '("~/.emacs.d/editor" "~/.emacs.d/lang")))
  (mapc (lambda (dir)
          (mapc 'load-file
                (directory-files-recursively dir "\\.el$")))
        dirs))
