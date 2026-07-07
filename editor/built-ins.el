;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook
  ((prog-mode text-mode) . visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t) ; no duplicates in kill ring
  (indent-tabs-mode nil))

;;;###autoload
(defun +scratch-buffer ()
  (interactive)
  (scratch-buffer)
  (lisp-interaction-mode))

(use-package advice
  :ensure nil
  :custom (ad-redefinition-action 'accept)) ; disable warnings from legacy advice system

(use-package files
  :ensure nil
  :custom
  (make-backup-files nil)
  (create-lockfiles nil)
  (auto-mode-case-fold nil)
  (auto-save-default nil)
  (find-file-suppress-same-file-warnings t)
  (large-file-warning-threshold (* 100 1024 1024))
  (confirm-nonexistent-file-or-buffer nil)
  :bind
  (:map +leader-map
        ("rr" . #'+restart-emacs))
  (:map +normal-mode-map
        ("q" . save-buffer)))

;;;###autoload
(defun +restart-emacs ()
  (interactive)
  (let ((confirm-kill-emacs nil))
    (restart-emacs)))

(use-package saveplace ; save location in files
  :ensure nil
  :hook (emacs-startup . save-place-mode)
  :custom
  (save-place-limit 600))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)) ; clean unnecessary whitespace before save

(use-package autorevert
  :ensure nil
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

(use-package savehist ; save command history
  :ensure nil
  :hook (emacs-startup . savehist-mode)
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(register-alist
                                   mark-ring global-mark-ring
                                   search-ring regexp-search-ring)))

;; Save the kill ring between restarts (remove non-string data)
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

(use-package recentf
  :ensure nil
  :defer 0.2
  :commands
  (consult-recentf)
  :config
  (recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 200))

(use-package desktop ; save sessions to a file
  :after (no-littering modal)
  :ensure nil
  :demand t
  :custom
  (desktop-restore-eager 10)
  (desktop-load-locked-desktop t) ; ignore desktop-lock files
  (desktop-base-file-name "emacs.desktop")
  :config
  (dolist (var '(+theme-rotate-dark-theme-index
                 +theme-rotate-light-theme-index
                 +theme-rotate-current-style))
    (add-to-list 'desktop-globals-to-save var))
  (desktop-save-mode))

(use-package electric-pair-mode
  :ensure nil
  :hook
  ((prog-mode text-mode) . electric-pair-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs '((?< . ?>)))))))

(use-package ediff-conflicts
  :ensure (:type file :main "~/.emacs.d/lisp/ediff-conflicts.el" :files ("ediff-conflicts.el"))
  :custom
  (+ediff-conflicts-files-function #'+jj-conflicted-files))

(use-package ediff
  :defer t
  :ensure nil
  :after ediff-conflicts
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :hook
  (ediff-startup . +ediff-select-first-difference)
  :config
  (setq ediff-diff-options ""))

;; (use-package outline-mode
;;   :ensure nil
;;   :bind
;;   (:map +leader-map
;;         ("oa" . #'outline-toggle-children)
;;         ("oj" . #'outline-next-heading)
;;         ("ok" . #'outline-previous-heading)
;;         ("or" . #'outline-show-all)
;;         ("om" . #'outline-hide-sublevels)))

(use-package repeat
  :ensure nil
  :custom
  (repeat-check-key nil)
  :hook (after-init . repeat-mode))

(use-package grep
  :ensure nil
  :bind
  (:map grep-mode-map
        (";" . #'compile-goto-error)
        ("q" . #'quit-window)))

(use-package grep-extras
  :ensure (:type file :main "~/.emacs.d/lisp/grep-extras.el" :files ("grep-extras.el"))
  :after grep
  :demand t
  :config
  (keymap-set grep-mode-map "d" #'+grep-export-dired))

(use-package menu-bar
  :ensure nil
  :bind
  (:map +leader-map
        ("td" . #'toggle-debug-on-error)))

(use-package occur
  :ensure nil
  :bind
  (:map occur-edit-mode-map
        ([remap save-buffer] . #'occur-cease-edit)))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

(use-package delsel
  :ensure nil
  :hook
  (emacs-startup . delete-selection-mode))

(use-package indent
  :ensure nil
  :init
  (setq indent-rigidly-map (make-sparse-keymap))
  :bind
  (:map indent-rigidly-map
        ("h" . #'indent-rigidly-left)
        ("H" . #'indent-rigidly-left-to-tab-stop)
        ("l" . #'indent-rigidly-right)
        ("L" . #'indent-rigidly-right-to-tab-stop)))

(defconst +elixir-mix-compilation-regexp
  (rx line-start
      (zero-or-more (not "\n"))
      (or line-start blank)
      (group (or "lib" "test")
             "/"
             (one-or-more (not (any ":\n")))
             ".ex"
             (optional "s"))
      ":"
      (group (one-or-more digit))
      (optional ":" (group (one-or-more digit)))
      (or ":" line-end))
  "Match existing local Elixir project paths in Mix compilation output.")

;;;###autoload
(defun +compilation-setup-mix-error-regexp (process)
  "Use Mix-specific compilation parsing in PROCESS' buffer."
  (when-let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (when (+compilation-mix-command-p)
        (setq-local compilation-error-regexp-alist-alist
                    (cons `(+elixir-mix
                            ,+elixir-mix-compilation-regexp
                            +elixir-compilation-file 2 3)
                          (default-value 'compilation-error-regexp-alist-alist)))
        (setq-local compilation-error-regexp-alist
                    (cons '+elixir-mix
                          (default-value 'compilation-error-regexp-alist)))))))

;;;###autoload
(defun +elixir-compilation-file ()
  "Return the current Mix compilation match if it exists on disk."
  (let* ((path (match-string-no-properties 1))
         (file (and path (expand-file-name path default-directory))))
    (when (and file (file-exists-p file))
      path)))

;;;###autoload
(defun +compilation-mix-command-p ()
  "Return non-nil when the current compilation command invokes Mix."
  (when-let ((command (car-safe compilation-arguments)))
    (string-match-p (rx (or string-start (not (any alnum "_" "-")))
                        "mix"
                        (or string-end (not (any alnum "_" "-"))))
                    command)))

(use-package compile
  :ensure nil
  :commands
  (compile)
  :config
  (require 'ansi-color)
  (add-hook 'compilation-start-hook #'+compilation-setup-mix-error-regexp)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package ibuffer
  :ensure nil
  :after modal
  :config
  (+modal-bind '+motion-mode '+motion-mode-map 'ibuffer-mode-hook
               '(("x" . ibuffer-do-kill-on-deletion-marks))))

(use-package subword-mode ;; enable `word' based commands to tread camel case text as separate words
  :ensure nil
  :hook
  ((java-mode java-ts-mode yaml-mode yaml-ts-mode) . subword-mode))

(use-package view-mode
  :ensure nil
  :custom
  (view-read-only t)
  :bind
  (:map view-mode-map
        ("v" . #'View-scroll-page-forward)
        ("p")
        ("n")
        ("<")
        (">")))

;;;###autoload
(defun +indent-rigidly-dwim ()
  (interactive)
  (unless (region-active-p)
    (+mark-whole-lines 1))
  (call-interactively #'indent-rigidly))

(use-package rect
  :ensure nil
  :bind
  (:map rectangle-mark-mode-map
        ("w" . #'kill-rectangle)
        ("i" . #'string-insert-rectangle)
        ([remap +change] . #'replace-rectangle)))
