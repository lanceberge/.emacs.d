;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook
  ((prog-mode text-mode) . visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t) ; no duplicates in kill ring
  (indent-tabs-mode nil)
  :bind
  ("M-T" . #'transpose-paragraphs)
  (:map +normal-mode-map
        ("RET" . #'newline)
        ("S-<return>" . #'insert-newline-above-dwim))
  (:map prog-mode-map
        ("C-g" . #'+keyboard-quit))
  (:map +normal-mode-map
        ("C-g" . #'+keyboard-quit))
  (:map text-mode-map
        ("C-g" . #'+keyboard-quit))
  (:map +leader-map
        ("bs" . #'+scratch-buffer)))

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
        ("re" . #'+restart-emacs))
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
  (desktop-save-mode)
  (desktop-read))

(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs '((?< . ?>))))))
  :config
  (remove-hook 'self-insert-uses-region-functions
               #'electric-pair-will-use-region)
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (if (char-equal c ?\") t (electric-pair-default-inhibit c))))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package ediff
  :defer t
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-diff-options ""))

(use-package outline-mode
  :ensure nil
  :bind
  (:map +leader-map
        ("oa" . #'outline-toggle-children)
        ("oj" . #'outline-next-heading)
        ("ok" . #'outline-previous-heading)
        ("or" . #'outline-show-all)
        ("om" . #'outline-hide-sublevels)))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

(use-package ispell
  :disabled t
  :ensure nil
  :custom
  (ispell-complete-word-dict t))

(use-package grep
  :ensure nil
  :bind
  (:map grep-mode-map
        (";" . #'compile-goto-error)
        ("q" . #'quit-window)))

(use-package menu-bar
  :ensure nil
  :bind
  (:map +leader-map
        ("td" . #'toggle-debug-on-error)))

(use-package occur
  :ensure nil
  :bind
  (:map occur-mode-map
        ("i" . #'occur-edit-mode))
  (:map occur-edit-mode-map
        ([remap save-buffer] . #'occur-cease-edit)))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  :bind
  (:map +leader-map
        ("b SPC" . #'+bookmark-file)))

;;;###autoload
(defun +bookmark-file ()
  (interactive)
  (when (buffer-file-name)
    (bookmark-set (file-name-nondirectory (buffer-file-name)) nil)))

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


(use-package compile
  :ensure nil
  :commands
  (compile)
  :config
  (require 'ansi-color)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(+elixir-exunit
                 "^[[:space:]]*\\([^:\n]+\\.exs?\\):\\([0-9]+\\):"
                 1 2))
  (add-to-list 'compilation-error-regexp-alist '+elixir-exunit)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package ibuffer
  :ensure nil
  :after modal
  :config
  (+modal-bind +motion-mode ibuffer-mode-hook
               "x" #'ibuffer-do-kill-on-deletion-marks))

(use-package subword-mode ;; enable `word' based commands to tread camel case text as separate words
  :ensure nil
  :hook
  ((java-mode java-ts-mode yaml-mode yaml-ts-mode) . subword-mode))

(use-package view-mode
  :ensure nil
  :hook (read-only-mode . view-mode)
  :bind
  (:map view-mode-map
        ("v" . #'View-scroll-page-forward)))

;;;###autoload
(defun +indent-rigidly-dwim ()
  (interactive)
  (unless (region-active-p)
    (+mark-whole-line))
  (call-interactively #'indent-rigidly))
