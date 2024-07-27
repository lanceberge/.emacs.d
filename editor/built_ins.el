;;; -*- lexical-binding: t -*-
(use-package simple
  :straight (:type built-in)
  :hook (after-init . global-visual-line-mode)
  :custom
  (idle-update-delay 1.0) ; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t)) ; no duplicates in kill ring

(use-package advice
  :straight (:type built-in)
  :defer t
  :custom (ad-redefinition-action 'accept)) ; disable warnings from legacy advice system

(use-package files
  :straight (:type built-in)
  :defer t
  :custom
  (make-backup-files nil)
  (create-lockfiles nil)
  (auto-mode-case-fold nil)
  (auto-save-default nil))

(use-package saveplace ; save location in files
  :straight (:type built-in)
  :hook (pre-command . save-place-mode))

(use-package whitespace
  :straight (:type built-in)
  :hook (before-save . whitespace-cleanup)) ; clean unnecessary whitespace before save

(use-package savehist ; save command history
  :straight (:type built-in)
  :hook (pre-command . savehist-mode)
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

(use-package recentf
  :straight (:type built-in)
  :defer-incrementally (easymenu tree-widget timer)
  :hook (pre-command . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 200))

(when (or IS-LINUX IS-MAC)
  (use-package flyspell ; spellcheck
    :straight (:type built-in)
    :hook
    (prog-mode . flyspell-prog-mode)
    (text-mode . flyspell-mode)
    :general
    ('(normal insert)
     "M-f" #'(flyspell-auto-correct-word :which-key "fix word"))))

(use-package bookmark
  :straight (:type built-in)
  :defer t)

(use-package calc
  :disabled t
  :straight (:type built-in)
  :general
  (my-leader-def
    "oc" #'(calc :which-key "calc")))

(use-package desktop ; save sessions to a file
  :defer t
  :straight (:type built-in)
  :custom
  (desktop-load-locked-desktop t) ; ignore desktop-lock files
  (desktop-path (list desktop-dirname))
  (desktop-save-mode 1)
  (desktop-base-file-name "emacs.desktop"))

(use-package project
  :straight (:type built-in)
  :general
  (my-leader-def
    "pk" #'(project-kill-buffers :which-key "find file")
    "pf" #'(project-find-file    :which-key "find file")))
