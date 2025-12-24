;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t) ; no duplicates in kill ring
  (indent-tabs-mode nil)
  :bind
  ("M-z" . #'zap-up-to-char)
  ("M-Z" . #'zap-to-char)
  ("M-T" . #'transpose-paragraphs)
  (:map meow-normal-state-keymap
        ("RET" . #'newline)
        ("S-<return>" . #'insert-newline-above-dwim))
  (:map prog-mode-map
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
  :bind
  (:map +leader-map
        ("re" . #'+restart-emacs))
  (:map meow-normal-state-keymap
        ("q" . save-buffer)))

;;;###autoload
(defun +restart-emacs ()
  (interactive)
  (let ((confirm-kill-emacs nil))
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

(use-package desktop ; save sessions to a file
  :ensure nil
  :demand t
  :custom
  (desktop-restore-eager 10)
  (desktop-load-locked-desktop t) ; ignore desktop-lock files
  (desktop-base-file-name "emacs.desktop")
  :config
  (dolist (var '(+themes-dark-theme-index +themes-light-theme-index +themes-current-style))
    (add-to-list 'desktop-globals-to-save var))
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
        ("ed" . #'toggle-debug-on-error)))

(use-package occur
  :ensure nil
  :bind
  (:map occur-mode-map
        ("i" . #'occur-edit-mode))
  (:map occur-edit-mode-map
        ([remap save-buffer] . #'occur-cease-edit)))

(use-package bookmark
  :ensure nil
  :bind
  (:map +leader-map
        ("b SPC" . #'+bookmark-file)))

;;;###autoload
(defun +bookmark-file ()
  (interactive)
  (when (buffer-file-name)
    (bookmark-set (file-name-nondirectory (buffer-file-name)) nil)))

(use-package register
  :ensure nil
  :bind
  (:map +leader-map
        ("rj" . #'jump-to-register)
        ("rw" . #'window-configuration-to-register)))
