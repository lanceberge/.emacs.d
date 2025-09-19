;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t) ; no duplicates in kill ring
  (indent-tabs-mode nil)
  :bind
  (:map meow-normal-state-keymap
        ("RET" . #'newline))
  (:map prog-mode-map
        ("C-g" . #'+isearch-clear-highlighting))
  (:map text-mode-map
        ("C-g" . #'+isearch-clear-highlighting)))

;;;###autoload
(defun +isearch-clear-highlighting ()
  (interactive)
  (lazy-highlight-cleanup)
  (isearch-exit)
  (keyboard-quit))

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
        ("re" . #'+restart-emacs)))

;;;###autoload
(defun +restart-emacs ()
  (interactive)
  (setq confirm-kill-emacs nil)
  (restart-emacs))

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
  :bind
  (:repeat-map outline-repeat-map
               ("a" . #'outline-toggle-children)
               ("o" . #'outline-show-subtree))
  (:map +leader-map
        ("oa" . #'outline-toggle-children)
        ("oo" . #'outline-show-subtree)
        ("or" . #'outline-show-all)
        ("om" . #'outline-hide-sublevels)))

(use-package repeat
  :ensure nil
  :hook ((prog-mode text-mode) . repeat-mode))

(use-package ispell
  :disabled t
  :ensure nil
  :custom
  (ispell-complete-word-dict t))

(use-package isearch
  :ensure nil
  :hook (isearch-mode-end . +isearch-exit-at-start)
  :bind
  (:map meow-normal-state-keymap
        ("n" . #'isearch-repeat-forward)
        ("N" . #'isearch-repeat-backward))
  (:map isearch-mode-map
        ("C-j" . #'isearch-repeat-forward)
        ("C-k" . #'isearch-repeat-backward)
        ("M-j" . #'avy-isearch))
  :config
  (setq search-nonincremental-instead nil))

(defun +isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (unless (or isearch-mode-end-hook-quit
              (bound-and-true-p isearch-suspended)
              (not isearch-forward)
              (not isearch-other-end)
              (and (boundp 'avy-command)
                   (eq avy-command 'avy-isearch)))
    (goto-char isearch-other-end)))

(defun +isearch-update-last-search (search-string)
  "Update isearch state to use SEARCH-STRING as the last search to be used by isearch-repeat."
  (when search-string
    (setq isearch-string search-string)
    (isearch-update-ring search-string isearch-regexp)
    (setq isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string ""))
    (setq isearch-case-fold-search isearch-last-case-fold-search)
    (setq isearch-success t)))

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
        ("ed" . #'toggle-debug-on-error)))
