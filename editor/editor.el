;;; -*- lexical-binding: t -*-
(use-package smartparens ; pair delimiters automatically and functions to work with delimiters
  :defer 0.1
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  (sp-escape-quotes-after-insert nil)
  (sp-show-pair-from-inside t)
  (sp-cancel-autoskip-on-backward-movement nil) ; quote pairs buggy otherwise
  :general
  ('normal
   ">" (general-key-dispatch #'evil-shift-right
         ")" #'(sp-forward-slurp-sexp :which-key "forward slurp")
         "(" #'(sp-backward-barf-sexp :which-key "backward barf"))
   "<" (general-key-dispatch #'evil-shift-left
         ")" #'(sp-forward-barf-sexp   :which-key "forward barf")
         "(" #'(sp-backward-slurp-sexp :which-key "backward slurp")))
  :config
  (smartparens-global-mode)
  (require 'smartparens-config) ; config for many languages

  ;; characters to not pair in org mode
  (eval-after-load 'smartparens-org '(progn
                                       (sp-local-pair 'org-mode "=" nil :actions nil)
                                       (sp-local-pair 'org-mode "~" nil :actions nil)
                                       (sp-local-pair 'org-mode "/" nil :actions nil)
                                       (sp-local-pair 'org-mode "_" nil :actions nil)
                                       (sp-local-pair 'org-mode "'" nil :actions nil)
                                       (sp-local-pair 'org-mode "*" nil :actions nil))))

(use-package format-all ; format code functions
  :hook
  (js2-mode . format-all-mode)
  :init
  (defvar +format-with-lsp nil)
  :general
  (my-leader-def
    :states 'normal
    "=" #'(+format/buffer :which-key "format")
    :config
    (setq-default format-all-formatters
                  '(("TypeScript" prettier)
                    ("JavaScript" prettier)))))

(use-package avy ; jump to things in files similar to easymotion for vim
  :general
  ('normal
   "go"      #'(avy-goto-char-2     :which-key "2-chars")
   "g SPC o" #'(avy-goto-char-timer :which-key "timer")))

(use-package flyspell-correct-ivy
  :general
  ('(normal insert)
   "C-j" #'flyspell-correct-wrapper))

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
  :defer t
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
  :defer t)
