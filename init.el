;;; -*- lexical-binding: t -*-
(if (version< emacs-version "27.1")
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(setq default-file-name-handler-alist file-name-handler-alist
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
      scroll-step 1

      ;; General
      apropos-do-all t     ; more extensive apropos searches
      completion-ignore-case t

      use-file-dialog nil
      use-dialog-box nil
      pop-up-frames nil

      ;; Disable bidirectional text rendering for performance
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      cursor-in-non-selected-windows nil)

(setq-default
 tab-width 4
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
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((doom-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state))))

(use-package general ; unified way to map keybindings; works with :general in use-package
  :custom (general-use-package-emit-autoloads t)
  :demand t
  :config
  (general-create-definer my-leader-def ; SPC prefixed bindings
    :states '(normal visual motion insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-c")

  (general-create-definer my-localleader-def ; , prefixed bindings
    :states '(normal visual motion insert emacs)
    :keymaps 'override
    :prefix ","
    :non-normal-prefix "C-,")

  (my-leader-def
    "fm" #'(general-describe-keybindings :which-key "list keybindings")))

(elpaca-wait)

(use-package which-key ; show keybindings following when a prefix is pressed
  :hook (pre-command . which-key-mode)
  :defer 0.1
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-min-display-lines 6)
  (which-key-add-column-padding 1)
  (which-key-sort-uppercase-first nil)
  :general
  ;; Set up all prefixes
  (my-localleader-def
    "m" '(:ignore t :which-key "Merge")
    "g" '(:ignore t :which-key "Miscellaneous")
    "t" '(:ignore t :which-key "Tramp ssh"))
  ('normal
   "[o" '(:ignore t :which-key "Toggle Off")
   "]o" '(:ignore t :which-key "Toggle On"))
  (my-leader-def
    "b"  '(:ignore t :which-key "Buffers")
    "e"  '(:ignore t :which-key "Elisp")
    "ei" '(:ignore t :which-key "Ein")
    "f"  '(:ignore t :which-key "Find")
    "fi" '(:ignore t :which-key "in directory")
    "g"  '(:ignore t :which-key "Git")
    "gf" '(:ignore t :which-key "Find")
    "gn" '(:ignore t :which-key "New")
    "o"  '(:ignore t :which-key "Open")
    "of" '(:ignore t :which-key "File")
    "oj" '(:ignore t :which-key "Org Journal")
    "on" '(:ignore t :which-key "Org Note")
    "ov" '(:ignore t :which-key "Vertical")
    "p"  '(:ignore t :which-key "Project")
    "s"  '(:ignore t :which-key "Yasnippet")
    "t"  '(:ignore t :which-key "Tab")

    "f SPC m" #'(which-key-show-top-level :which-key "keybinding")))

(add-hook 'after-init-hook ; show startup time
          (lambda ()
            "show the startup time"
            (when (require 'time-date nil t)
              (message "Emacs init time: %.2f seconds."
                       (time-to-seconds (time-since emacs-load-start-time))))))

(add-hook 'emacs-startup-hook ; reset garbage collection settings and file-name-handler-alist
          (lambda ()
            "raise the garbage collection threshold to defer garbage collection
           and unset file-name-handler-alist"
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;; Raise gc threshold while minibuffer is active
(defun doom-defer-garbage-collection-h ()
  "Defer garbage collection by setting it to the largest possible number"
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore the garbage collection threshold"
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 107772160))))

;; decrease garbage collection when using minibuffer
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook  #'doom-restore-garbage-collection-h)

(use-package gcmh			; Garbage collect in idle time
  :defer 2.0
  :commands gcmh-idle-garbage-collect
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 17772160)
  :config
  (gcmh-mode)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))

;; Load config files recursively
(let ((dirs '("~/.emacs.d/editor" "~/.emacs.d/lang")))
  (mapc (lambda (dir)
          (mapc 'load-file
                (directory-files-recursively dir "\\.el$")))
        dirs))

(setq custom-file "~/.emacs.d/custom.el")
