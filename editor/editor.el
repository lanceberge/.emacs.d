;;; -*- lexical-binding: t -*-
(use-package format-all ; format code functions
  :hook
  (prog-mode . format-all-mode)
  (web-mode  . format-all-mode)
  :init
  (defvar +format-with-lsp nil)
  :general
  (my-leader-def
    :states 'normal
    "=" #'(+format/buffer :which-key "format"))
  :config
  (setq-default format-all-formatters
                '(("TypeScript" prettier)
                  ("svelte"     prettier)
                  ("HTML"       prettier)
                  ("JavaScript" prettier))))

(use-package avy ; jump to things in files similar to easymotion for vim
  :general
  ('evil-operator-state-map
   "go" #'(avy-goto-char-2 :which-key "goto char"))
  ('normal
   "go"      #'(avy-goto-char-2     :which-key "2-chars")
   "g SPC o" #'(avy-goto-char-timer :which-key "timer"))
  )

(use-package define-word
  :general
  ('(normal insert)
   "C-y" #'define-word-at-point))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-auto-revert-buffer) ; don't prompt to revert
  (dired-recursive-copies 'always)
  :general
  ('normal
   "-"  #'(dired-jump :which-key "open dired"))
  :config
  (evil-collection-init 'dired)

  (put 'dired-find-alternate-file 'disabled nil)

  (general-def 'normal dired-mode-map
    "RET" nil
    ";"   #'dired-find-alternate-file ; select a directory in the same buffer
    "i"   #'+dired/edit
    "-"   #'+dired/up-dir))

(use-package dired-x
  :straight (:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired

(use-package helpful ; better help menu
  :defer 0.3
  :general
  ('normal
   "gp" #'helpful-at-point)
  ('normal helpful-mode-map
           "q" #'quit-window)

  ([remap describe-command] #'helpful-command
   [remap describe-key]     #'helpful-key
   [remap describe-symbol]  #'helpful-symbol)
  :config
  (evil-collection-inhibit-insert-state 'helpful-mode-map))

(use-package undo-tree ; Persistent Undos
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-limit 10000)
  (undo-tree-auto-save-history t)
  (evil-undo-system 'undo-tree)
  :general
  (my-leader-def
    "fu" #'(undo-tree-visualize :which-key "undo")))

(use-package exec-path-from-shell ; Use system $PATH variable for eshell, commands, etc.
  :hook (after-init . (lambda () (setq exec-path-from-shell-arguments '("-l"))
                        (exec-path-from-shell-initialize))))

(use-package google-this
  :general
  (my-localleader-def
    "gt" #'google-this-symbol
    "gs" #'google-this-search)

  (my-localleader-def
    :states 'visual
    "gt" (lambda () (interactive)
           (google-this-region t t))))

(use-package chatgpt-shell
  :general
  ('normal chatgpt-shell-mode-map
           "RET" #'shell-maker-submit)
  (my-localleader-def
    "gc" #'chatgpt-shell)

  :config
  (setq chatgpt-shell-root-path (no-littering-expand-var-file-name "chatgpt/"))
  (setq chatgpt-shell-model-version "gpt-3.5-turbo")
  (setq chatgpt-shell-openai-key (read-file-contents "~/secrets/gpt_api_key")))

(use-package ace-link
  :general
  ('normal
   "g SPC l" #'(ace-link :which-key "goto link")))


(use-package wgrep
  :general
  ('normal grep-mode-map
           "i" #'wgrep-change-to-wgrep-mode)
  ('wgrep-mode-map
   [remap evil-write] 'wgrep-save-all-buffers
   [remap evil-save-modified-and-close] (lambda () (interactive)
                                          (wgrep-save-all-buffers)
                                          (evil-quit)))
  :defer t)
(use-package popper
  :defer 0.3
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*xref\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "^\\*vterm.*\\*$"
     "^\\*eldoc"
     "\\*Warnings\\*"
     helpful-mode
     help-mode
     compilation-mode))
  :general
  (my-leader-def
    "'" #'popper-toggle)

  ('evil-window-map
   "p" #'popper-toggle-type)

  ('(normal insert visual)
   "C-'" #'popper-toggle)

  :config
  (popper-mode +1))
