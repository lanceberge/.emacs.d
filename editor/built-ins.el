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
  :hook (pre-command . save-place-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)) ; clean unnecessary whitespace before save

(use-package autorevert
  :ensure nil
  :hook (pre-command . global-auto-revert-mode))

(use-package savehist ; save command history
  :ensure nil
  :hook (pre-command . savehist-mode)
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

(use-package recentf
  :ensure nil
  :defer-incrementally (easymenu tree-widget timer)
  :hook (pre-command . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 200))

(when (or IS-LINUX IS-MAC)
  (use-package flyspell ; spellcheck
    :ensure nil
    :hook
    (prog-mode . flyspell-prog-mode)
    (text-mode . flyspell-mode)
    :general
    ('normal
     "[os" (defun +flyspell-off () (interactive) (flyspell-mode -1) :which-key "flyspell")
     "]os" (defun +flyspell-on  () (interactive) (flyspell-mode t) :which-key "flyspell"))
    ('(normal insert)
     "M-y" #'(flyspell-auto-correct-word :which-key "fix word"))))

(use-package bookmark
  :ensure nil
  :defer t)

(use-package calc
  :disabled t
  :ensure nil
  :general
  (my-leader-def
    "oc" #'(calc :which-key "calc")))

;; TODO
(use-package desktop ; save sessions to a file
  :disabled t
  :ensure nil
  :custom
  (desktop-load-locked-desktop t) ; ignore desktop-lock files
  (desktop-path (list desktop-dirname))
  (desktop-save-mode 1)
  (desktop-base-file-name "emacs.desktop"))


(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
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
  (setq ediff-diff-options "")
  (add-hook 'ediff-prepare-buffer-hook #'evil-open-folds))

(use-package outline-mode
  :ensure nil
  :general
  ('normal
   "za" #'(outline-toggle-children :which-key "toggle fold")
   "zo" #'(outline-show-subtree :which-key "fully open")
   "zc" #'(outline-hide-subtree :which-key "fully fold")
   "zk" #'(org-backward-element :which-key "go up an outline")
   "zj" #'(org-forward-element :which-key "go down an outline")
   "zr" #'(outline-show-all :which-key "close all folds")
   "zm" #'(outline-hide-body :which-key "open all folds")))

(use-package ispell
  :defer t
  :ensure nil
  :custom
  (ispell-complete-word-dict t))

(use-package occur
  :ensure nil
  :hook
  (occur-mode . (lambda ()
                  (evil-local-mode)
                  (evil-normal-state)))
  :general
  ('normal 'occur-mode-map
           "q" #'quit-window))

(use-package isearch
  :ensure nil
  :general
  ('normal
   "/" #'isearch-forward
   "?" #'isearch-backward)

  ('isearch-mode-map
   "C-j" #'isearch-repeat-forward
   "C-k" #'isearch-repeat-backward
   "M-j" #'isearch-repeat-forward
   "M-k" #'isearch-repeat-backward
   "C-g" #'isearch-exit
   ";" #'isearch-exit))

(use-package grep-mode
  :ensure nil
  :hook
  (grep-mode . evil-normal-state)
  :general
  ('normal 'grep-mode-map
           ";" #'compile-goto-error
           "q" #'quit-window))

(use-package xref
  :ensure nil
  :general
  ('normal 'prog-mode-map
           "gr" #'xref-find-references :which-key "find references"))

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))
