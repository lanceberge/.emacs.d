;;; -*- lexical-binding: t -*-
(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (idle-update-delay 1.0)	; slow down how often emacs updates its ui
  (kill-do-not-save-duplicates t)
  (indent-tabs-mode nil)) ; no duplicates in kill ring

(use-package advice
  :ensure nil
  :defer t
  :custom (ad-redefinition-action 'accept)) ; disable warnings from legacy advice system

(use-package files
  :ensure nil
  :defer t
  :custom
  (make-backup-files nil)
  (create-lockfiles nil)
  (auto-mode-case-fold nil)
  (auto-save-default nil))

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
  :general
  ('meow-insert-state-keymap
   "M-y" #'flyspell-auto-correct-word))

(use-package bookmark
  :ensure nil
  :defer t)

(use-package desktop ; save sessions to a file
  :ensure nil
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
  :defer t
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-use-faces nil)
  :config
  (setq ediff-diff-options ""))

(use-package outline-mode
  :ensure nil
  :defer t
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
  :defer t
  :ensure nil
  :custom
  (ispell-complete-word-dict t))

(use-package occur
  :ensure nil
  :general
  ('meow-normal-state-keymap 'occur-mode-map
                             "q" #'quit-window))
(use-package isearch
  :ensure nil
  :general
  ('meow-normal-state-keymap
   "/" #'isearch-forward
   "?" #'isearch-backward)

  ('isearch-mode-map
   "C-j" #'isearch-repeat-forward
   "C-k" #'isearch-repeat-backward
   "M-j" #'isearch-repeat-forward
   "M-k" #'isearch-repeat-backward
   "C-g" #'isearch-exit)
  :config
  (setq search-nonincremental-instead nil))

(use-package grep-mode
  :ensure nil
  :general
  ('meow-normal-state-keymap 'grep-mode-map
                             ";" #'compile-goto-error
                             "q" #'quit-window))

(use-package xref
  :ensure nil
  :general
  ('meow-normal-state-keymap 'prog-mode-map
                             "gr" #'xref-find-references :which-key "find references"))

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))
