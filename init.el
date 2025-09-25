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
        highlight-nonselected-windows nil
        ffap-machine-p-known 'reject ; don't ping things that look like domain names
        bidi-inhibit-bpa t
        fast-but-imprecise-scrolling t ; faster scrolling over unfontified regions

        ;; Scrolling
        scroll-conservatively 1000
        scroll-margin 4
        scroll-preserve-screen-position t
        scroll-step 1

        ;; General
        apropos-do-all t ; more extensive apropos searches
        completion-ignore-case t
        kill-whole-line t

        global-mark-ring-max 64

        use-file-dialog nil
        use-dialog-box nil
        pop-up-frames nil

        debugger-stack-frame-as-list t

        history-delete-duplicates t

        ;; Disable bidirectional text rendering for performance
        bidi-display-reordering 'left-to-right
        bidi-paragraph-direction 'left-to-right
        cursor-in-non-selected-windows nil)

(when IS-MAC
  (dolist (path '("/opt/homebrew/opt/llvm/bin/"
                  "~/go/bin"
                  "/Users/lance/.config/herd-lite/bin/"
                  "/opt/homebrew/opt/openjdk/bin/"
                  "~/.config/composer/vendor/bin/"))
    (add-to-list 'exec-path path))
  (setenv "PATH" (mapconcat 'identity exec-path path-separator)))

(setq-default
 tab-width 4
 require-final-newline t
 indent-tabs-mode nil) ; tabs are converted to spaces

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

;; https://github.com/hlissner/doom-emacs/blob/42a21dffddeee57d84e82a9f0b65d1b0cba2b2af/core/core.el#L353
(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
    here may cause noticeable pauses, so it's recommended you break them up into
    sub-packages. For example, `org' is comprised of many packages, and can be
    broken up into:
      (doom-load-packages-incrementally
       '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))
    This is already done by the lang/org module, however.
    If you want to disable incremental loading altogether, either remove
    `doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
    `doom-incremental-first-idle-timer' to nil.")

(defvar doom-incremental-first-idle-timer 2.0
  "How long (in idle seconds) until incremental loading starts.
    Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar doom-incremental-load-immediately nil
  ;; (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

;;;###autoload
(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.
    If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
    intervals."
  (if (not now)
      (appendq! doom-incremental-packages packages)
    (while packages
      (let ((req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (gc-cons-threshold most-positive-fixnum)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load '%s' package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer doom-incremental-idle-timer
                                 nil #'doom-load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

;;;###autoload
(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.
    If this is a daemon session, load them all immediately instead."
  (if doom-incremental-load-immediately
      (mapc #'require (cdr doom-incremental-packages))
    (when (numberp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)

;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
;;
;;   :after-call SYMBOL|LIST
;;   :defer-incrementally SYMBOL|LIST|t
;;
;; Check out `use-package!'s documentation for more about these two.
(eval-when-compile
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
;;;###autoload
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((doom-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state))))

(defvar +leader-map (make-sparse-keymap))
(defvar +leader2-map (make-sparse-keymap))
(defvar +leader3-map (make-sparse-keymap))

(use-package meow
  :demand t
  :hook (after-init . meow-global-mode)
  :bind
  (:repeat-map meow-mark-repeat-map
               ("[" . #'+backward-global-mark)
               ("]" . #'+forward-global-mark))
  :bind
  (:map meow-motion-state-keymap
        ("q" . #'meow-quit)
        ("h" . #'meow-left)
        ("H" . #'meow-left-expand)
        ("l" . #'meow-right)
        ("SPC" . nil)
        ("L" . #'meow-right-expand)
        ("j" . #'meow-next)
        ("SPC" . nil)
        ("g" . #'meow-cancel-selection)
        ("[" . #'meow-beginning-of-thing)
        ("]" . #'meow-end-of-thing)
        ("k" . #'meow-prev)
        ("C-u" . #'scroll-down)
        ("C-d" . #'scroll-up)
        ("x" . #'meow-line)
        ("." . #'meow-bounds-of-thing)
        ("," . #'meow-inner-of-thing)
        ("y" . #'meow-save))
  (:map +leader3-map
        ("[" . #'+backward-global-mark)
        ("]" . #'+forward-global-mark))
  (:map meow-normal-state-keymap
        ("p" . (lambda () (interactive)
                 (if (region-active-p)
                     (meow-replace)
                   (meow-yank))))
        ("-" . #'negative-argument)
        ("SPC" . nil)
        (";" . #'meow-reverse)
        ("." . #'meow-bounds-of-thing)
        ("," . #'meow-inner-of-thing)
        ("C-u" . #'scroll-down)
        ("C-d" . #'scroll-up)
        ("q" . #'save-buffer)
        ("[" . #'meow-beginning-of-thing)
        ("M-[" . #'meow-pop-to-mark)
        ("M-]" . #'meow-unpop-to-mark)
        ("d" . #'delete-char)
        ("]" . #'meow-end-of-thing)
        ("c" . #'+meow-change)
        ("a" . #'meow-append)
        ("A" . #'+meow-open-below)
        ("b" . #'meow-back-word)
        ("B" . #'meow-back-symbol)
        ("D" . #'meow-backward-delete)
        ("e" . #'meow-next-word)
        ("E" . #'meow-next-symbol)
        ("f" . #'meow-find)
        ("g" . #'+meow-cancel-selection)
        ("G" . #'meow-grab)
        ("h" . #'meow-left)
        ("H" . #'meow-left-expand)
        ("i" . #'meow-insert)
        ("I" . #'meow-open-above)
        ("j" . #'meow-next)
        ("J" . #'meow-next-expand)
        ("k" . #'meow-prev)
        ("K" . #'meow-prev-expand)
        ("l" . #'meow-right)
        ("L" . #'meow-right-expand)
        ("m" . #'meow-join)
        ("O" . #'meow-to-block)
        ("r" . #'+meow-replace-char)
        ("P" . #'meow-swap-grab)
        ("s" . #'meow-kill)
        ("t" . #'meow-till)
        ("v" . #'+meow-visit)
        ("w" . #'meow-mark-word)
        ("W" . #'meow-mark-symbol)
        ("x" . #'meow-line)
        ("X" . #'meow-goto-line)
        ("y" . #'meow-save)
        ("Y" . #'meow-sync-grab)
        ("z" . #'+meow-pop-selection)
        ("<escape>" . #'keyboard-quit))
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-state-mode-alist '((normal . meow-normal-mode)
                                (insert . meow-insert-mode)
                                (motion . meow-motion-mode)
                                (beacon . meow-beacon-mode)))
  (setq meow-use-cursor-position-hack t)
  (setq meow--kbd-forward-line #'next-line
        meow--kbd-backward-line #'previous-line
        meow--kbd-forward-char #'forward-char)

  (dotimes (i 10)
    (define-key meow-normal-state-keymap
                (number-to-string i)
                `(lambda ()
                   (interactive)
                   (if (region-active-p)
                       (meow-expand ,i)
                     (progn
                       (setq prefix-arg ,i)
                       (universal-argument--mode))))))

  (defvar-local +meow-desired-state nil)

;;;###autoload
  (defun +meow-set-desired-state (state)
    (setq-local +meow-desired-state state))

;;;###autoload
  (defun +meow-mode-get-state-advice (orig-func &rest args)
    (if +meow-desired-state
        +meow-desired-state
      (apply orig-func args)))

  (advice-add 'meow--mode-get-state :around #'+meow-mode-get-state-advice)

;;;###autoload
  (defun +meow-motion-mode ()
    (+meow-set-desired-state 'motion))

  (meow-setup-indicator)

  (setq meow-use-clipboard t))

(elpaca-wait)

(define-key meow-normal-state-keymap (kbd "SPC") +leader-map)
(define-key meow-motion-state-keymap (kbd "SPC") +leader-map)
(define-key meow-normal-state-keymap (kbd "'") +leader2-map)
(define-key meow-motion-state-keymap (kbd "'") +leader2-map)
(define-key meow-normal-state-keymap (kbd "`") +leader3-map)
(define-key meow-motion-state-keymap (kbd "`") +leader3-map)

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
  :defer 4.0
  :ensure nil
  :config
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-start)))

;; Load config files recursively
(let ((dirs '("~/.emacs.d/editor" "~/.emacs.d/lang")))
  (mapc (lambda (dir)
          (mapc 'load-file
                (directory-files-recursively dir "\\.el$")))
        dirs))
