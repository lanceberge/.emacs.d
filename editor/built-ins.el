;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t) ; no duplicates in kill ring
  (indent-tabs-mode nil))

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
  :bind
  (:map +leader-map
        ("re" . #'+restart-emacs))
  :config
  (defun +restart-emacs ()
    (interactive)
    (let ((vterm-buffer (get-buffer "*vterm*")))
      (if vterm-buffer
          (kill-buffer )))
    (setq confirm-kill-emacs nil)
    (restart-emacs)))

(use-package saveplace ; save location in files
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)) ; clean unnecessary whitespace before save

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package savehist ; save command history
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

(use-package recentf
  :ensure nil
  :defer-incrementally (easymenu tree-widget timer)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 200))

(use-package flyspell
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :bind
  (:map meow-insert-state-keymap
        ("C-y" . #'flyspell-auto-correct-word)))

(use-package bookmark
  :ensure nil)

(use-package desktop ; save sessions to a file
  :ensure nil
  :demand t
  :custom
  (desktop-load-locked-desktop t) ; ignore desktop-lock files
  (desktop-base-file-name "emacs.desktop")
  :config
  (desktop-save-mode))

(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs '((?< . ?>))))))
  :config
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (if (char-equal c ?\") t (electric-pair-default-inhibit c))))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-use-faces nil)
  :config
  (setq ediff-diff-options ""))

(use-package outline-mode
  :ensure nil
  ;; TODO
  ;; (meow-normal-define-key
  ;;  '("za" . outline-toggle-children)
  ;;  '("zo" . outline-show-subtree)
  ;;  '("zc" . outline-hide-subtree)
  ;;  '("zk" . org-backward-element)
  ;;  '("zj" . org-forward-element)
  ;;  '("zr" . outline-show-all)
  ;;  '("zm" . outline-hide-body))
  )

(use-package ispell
  :ensure nil
  :custom
  (ispell-complete-word-dict t))

(use-package isearch
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("/" . #'isearch-forward)
        ("?" . #'isearch-backward))
  (:map isearch-mode-map
        ("C-j" . #'isearch-repeat-forward)
        ("C-k" . #'isearch-repeat-backward)
        ("M-j" . #'isearch-repeat-forward)
        ("M-k" . #'isearch-repeat-backward)
        ("C-g" . #'isearch-exit))
  :config
  (setq search-nonincremental-instead nil))

(use-package grep
  :ensure nil
  :bind
  (:map grep-mode-map
        (";" . #'compile-goto-error)
        ("q" . #'quit-window)))

(use-package xref
  :ensure nil)

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("Q" . #'+kmacro-record-or-end))
  (:map meow-normal-state-keymap
        ("C-q" . #'kmacro-call-macro))
  (:map meow-insert-state-keymap
        ("C-q" . #'kmacro-call-macro)))

(use-package menu-bar
  :ensure nil
  :bind
  (:map +leader-map
        ("ed" . #'toggle-debug-on-error)))

(use-package window
  :ensure nil
  :bind
  (:map +leader-map
        ("wo" . #'delete-other-windows)
        ("wd" . #'delete-window)
        ("wj" . #'other-window)
        ("ws" . #'split-window-below)
        ("wv" . #'split-window-right)))

(defvar +kmacro-recording nil)

(defun +kmacro-record-or-end ()
  (interactive)
  (if +kmacro-recording
      (call-interactively #'kmacro-end-macro)
    (call-interactively #'kmacro-start-macro))
  (setq +kmacro-recording (not +kmacro-recording)))
