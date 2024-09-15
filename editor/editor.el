;;; -*- lexical-binding: t -*-
(use-package format-all ; format code functions
  :hook
  (prog-mode . format-all-mode)
  (web-mode . format-all-mode)
  (json-mode . format-all-mode)
  :general
  (my-leader-def
    :states 'normal
    "=" #'(+format/buffer :which-key "format"))
  ('normal
   "[of" (defun +format-all-off () (interactive)
                (format-all-mode -1) :which-key "format-all")
   "]of" (defun +format-all-on  () (interactive)
                (format-all-mode 1) :which-key "format all"))
  :config
  (defvar +format-with-lsp nil)
  (setq-default format-all-formatters format-all-default-formatters))

(use-package avy ; jump to things in files similar to easymotion for vim
  :custom
  (avy-keys '(?d ?j ?s ?k ?a ?l))
  :general
  ('evil-operator-state-map
   "\\" #'(avy-goto-char-timer :which-key "goto char")
   "go" #'(avy-goto-char-2 :which-key "goto char"))

  ('(normal insert)
   "M-i" #'(avy-goto-char-timer :which-key "goto char"))

  ('isearch-mode-map
   "M-i" #'evil-avy-isearch)

  ('normal
   "\\" #'(avy-goto-char-timer :which-key "2-chars")
   "go" #'(avy-goto-char-2 :which-key "2-chars")
   "g SPC o" #'(evil-avy-isearch :which-key "timer")
   "g SPC m" #'(avy-move-line :which-key "move line"))
  :config
  (evil-define-avy-motion avy-isearch inclusive)
  ;; https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-embark (pt)
    "Perform an embark action on the avy target without moving point to it"
    (unwind-protect
        (save-excursion
          (avy-action-embark-move pt))
      (select-window
       (cdr (ring-ref avy-ring 0)))) t)

  (defun avy-action-embark-move (pt)
    "Perform an embark action on the avy target and move the point to it"
    (goto-char pt)
    (embark-act))

  (setq avy-dispatch-alist
        (list
         (cons ?\s 'avy-action-embark-move)
         (cons ?, 'avy-action-embark))))

(use-package embark
  :general
  ('(insert normal) global-map
   "M-." #'embark-act
   "M-," #'embark-export)
  ('embark-general-map
   "$" nil
   ";" #'flyspell-auto-correct-word
   "y" #'define-word-at-point
   "d" #'embark-find-definition
   "g" #'google-this-word)
  ('embark-identifier-map
   "." #'lsp-execute-code-action))

(use-package define-word
  :commands (define-word-at-point))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer) ; don't prompt to revert
  (dired-recursive-copies 'always)
  :general
  ('normal
   "-" #'(dired-jump :which-key "open dired"))
  :config
  (evil-collection-init 'dired)
  (put 'dired-find-alternate-file 'disabled nil)

  (general-def 'normal dired-mode-map
    "RET" nil
    ";" #'dired-find-alternate-file ; select a directory in the same buffer
    "i" #'+dired/edit
    "-" #'+dired/up-dir))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired

(use-package helpful ; better help menu
  :defer 0.7
  :general
  ('normal
   "gp" #'helpful-at-point)
  ('normal helpful-mode-map
           "q" #'quit-window)

  ([remap describe-command] #'helpful-command
   [remap describe-key] #'helpful-key
   [remap describe-variable] #'helpful-variable
   [remap describe-function] #'helpful-function
   [remap describe-symbol] #'helpful-symbol)
  :config
  (evil-collection-inhibit-insert-state 'helpful-mode-map))

(use-package undo-tree ; Persistent Undos
  :defer 0.1
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :custom
  (undo-limit 10000)
  (undo-tree-auto-save-history t)
  (evil-undo-system 'undo-tree)
  :general
  (my-leader-def
    "fu" #'(undo-tree-visualize :which-key "undo"))
  :config
  (global-undo-tree-mode))

(if (version< emacs-version "29.1")
    (use-package exec-path-from-shell ; Use system $PATH variable for eshell, commands, etc.
      :custom
      (exec-path-from-shell-arguments '("-l"))
      (sh-shell-file "/usr/bin/zsh")
      (shell-file-name "zsh")
      (exec-path (append exec-path '("~/miniconda3/bin")))
      :hook
      (after-init . (lambda () (setq exec-path-from-shell-arguments '("-l"))
                      (exec-path-from-shell-copy-env "PYTHONPATH")
                      (exec-path-from-shell-initialize)))))

(use-package google-this
  :general
  (my-localleader-def
    "gt" #'google-this-symbol
    "gs" #'google-this-search)

  (my-localleader-def
    :states 'visual
    "gt" (defun +google-this () (interactive)
                (google-this-region t t) :which-key "google this")))

(use-package gptel
  :custom
  (gptel-model "claude-3-5-sonnet-20240620")
  (gptel-default-mode 'org-mode)
  :general
  (my-localleader-def
    "gc" #'gptel)

  ('gptel-mode-map
   "RET" #'gptel-send
   "S-<return>" #'newline)
  :config
  (defun gptel-api-key ()
    (read-file-contents "~/secrets/claude_key"))
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(use-package elysium
  :ensure (:host github :repo "lanceberge/elysium")
  :general
  ('visual
   "sq" #'elysium-query)
  (my-localleader-def
    "sq" #'elysium-query
    "so" #'elysium-keep-all-suggested-changes
    "sm" #'elysium-discard-all-suggested-changes
    "st" #'elysium-toggle-window)
  ('(insert normal) 'gptel-mode-map
   "C-<return>" #'elysium-query))

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
   [remap evil-save-modified-and-close]
   (defun +wgrep-save-and-quit () (interactive)
          (wgrep-save-all-buffers)
          (evil-quit) :which-key "save and quit"))
  :defer t)

(use-package popper
  :defer 0.3
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*xref\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "^\\*lsp-help.*\\*$"
     "^\\*eldoc"
     "^\\*Embark Collect:"
     "^\\*Embark Export:"
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

(use-package restart-emacs
  :general
  (my-leader-def
    "re" #'(+restart-emacs :which-key "restart emacs"))
  :config
  (defun +restart-emacs ()
    (interactive)
    (setq confirm-kill-emacs nil)
    (restart-emacs)))
