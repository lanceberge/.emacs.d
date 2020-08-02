;; Initial Settings
(setq user-full-name "Lance Bergeron"
      user-mail-address "bergeron.lance6@gmail.com"
      inhibit-startup-screen t)

(menu-bar-mode -1) ; no menu bar
(toggle-scroll-bar -1) ; no scroll bar
(tool-bar-mode -1) ; no tool bar
(set-frame-font "DejaVu Sans Mono") ; font
(set-face-attribute 'default nil :height 110) ; font size
(menu-bar-display-line-numbers-mode 'visual) ; relative line numbers
(global-display-line-numbers-mode 1) ; always show line numbers
(toggle-frame-fullscreen)

;; Keybindings
(use-package which-key
  :config
  (which-key-mode))

(use-package general
  :config
  (general-create-definer my-leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-evil-setup)
  (my-leader-def
    "f"  '(:ignore t :which-key "Find")
    "fm" '(general-describe-keybindings :which-key "list keybindings")
    "q"  '(save-buffers-kill-emacs :which-key "save buffers & quit emacs")
    "f." '(dired-jump :which-key "open dired")
    "fd" '(dired :which-key "navigate to a directory")
    "oa" '(org-agenda :which-key "org agenda")))

;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-search-wrap t
	evil-want-keybinding nil)
  :general
  (evil-ex-completion-map ";" 'exit-minibuffer)
  ('(normal visual motion)
   ";" 'evil-ex
   ":" 'evil-repeat-find-char
   "H" "^"
   "L" "$")
  (general-nmap
    ;; Vim-like Macros
    "SPC =" "mzgg=G`z"
    ;; Remaps
    "gm" 'evil-execute-macro
    "]b" '(evil-next-buffer :which-key "next buffer")
    "[b" '(evil-prev-buffer :which-key "previous buffer"))
  (my-leader-def
    "h" (general-simulate-key "C-h")
    ;; Windows
    "w"  '(:ignore t :which-key "Windows")
    "w" (general-simulate-key "C-w")
    ;; Buffers
    "b"  '(:ignore t :which-key "Buffers")
    "bs" '(evil-write :which-key "write file")
    "bd" '(evil-delete-buffer :which-key "delete buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
    "bS" '(evil-write-all :which-key "write all buffers"))
  :config
  (define-key evil-window-map "d" 'evil-quit)
  (define-key evil-window-map "q" 'evil-save-modified-and-close)
  (evil-mode 1))

;; 2 character searches with s (ala vim-sneak)
(use-package evil-snipe
  :hook ((prog-mode text-mode) . evil-snipe-local-mode)
  :init
  (setq evil-snipe-smart-case t))

;; Evil everywhere
(use-package evil-collection
  :config
  (evil-collection-init))

;; s as an operator for surrounding
(use-package evil-surround
  :hook ((prog-mode text-mode) . evil-surround-mode))

;; gc as an operator to comment
(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode))

;; jk to leave insert mode
(use-package evil-escape
  :hook ((prog-mode text-mode) . evil-escape-mode)
  :config
  (setq evil-escape-key-sequence "jk"
	evil-escape-delay 0.25))

;; gl as an operator to left-align, gL to right-align
(use-package evil-lion
  :hook ((prog-mode text-mode) . evil-lion-mode))

;; Persistent Undos
(use-package undo-tree
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :init
  (setq undo-limit 10000
	undo-tree-auto-save-history t))

;; z - prefixed folding options like vim
(use-package evil-vimish-fold
  :hook ((prog-mode text-mode) . evil-vimish-fold-mode))

(use-package ivy
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode)
  (use-package counsel
    :general
    (my-leader-def
      "."   '(counsel-find-file :which-key "find file")
      "fb"  '(ivy-switch-buffer :which-key "switch buffer")
      "fr"  '(counsel-recentf :which-key "find recent files")
      "fl"  '(counsel-grep-or-swiper :which-key "find line"))
    ;; "ff" '(counsel-locate) :which-key)

    :config
    (counsel-mode)
    (use-package counsel-projectile
    :general
    (my-leader-def
      "pp" '(counsel-projectile-switch-project :which-key "switch project")
      "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer")))))
;; (use-package flx :ensure t))

;; Terminal Emulator
(use-package vterm
  :general
  (my-leader-def
    "o"   '(:ignore t :which-key "Open")
    "ot"  '(vterm :which-key "open vterm")
    "ovt" '(vterm-other-window) :which-key "open vterm in vsplit")
  :config
  (setq vterm-kill-buffer-on-exit t))

;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :general
  (my-leader-def
    "fs" 'yas-describe-tables)
  :config
  (use-package yasnippet-snippets))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)) ; don't pair ' in elisp mode

;; Provides the restart-emacs function
(use-package restart-emacs
  :general
  (my-leader-def
    "e"  '(:ignore t :which-key "Emacs Commands")
    "er" '(restart-emacs :which-key "restart emacs"))
  :config
  (setq restart-emacs-restore-frames t)) ;; Restore frames on restart

;; Autopair parentheses
;; (use-package autopair :ensure t
;;   :config
;;   (autopair-global-mode))

;; Magit
(use-package evil-magit
  :general
  (my-leader-def
    "g"   '(:ignore t :which-key "Magit")
    "gs"  '(magit-status :which-key "status")
    "gb"  '(magit-branch-checkout :which-key "checkout branch")
    "gB"  '(magit-blame-addition :which-key "blame")
    "gc"  '(magit-clone :which-key "clone")
    "gd"  '(magit-file-delete :which-key "delete file")
    "gF"  '(magit-fetch :which-key "fetch")
    "gG"  '(magit-status-here :which-key "status here")
    "gl"  '(magit-log :which-key "log")
    "gS"  '(magit-stage-file :which-key "stage file")
    "gU"  '(magit-unstage-file :which-key "unstage file")
    "gn"  '(:ignore t :which-key "New")
    "gnb" '(magit-branch-and-checkout :which-key "branch")
    "gnc" '(magit-commit-create :which-key "commit")
    "gnf" '(magit-commit-fixup :which-key "fixup commit")
    "gnd" '(magit-init :which-key "init")
    "gf"  '(:ignore t :which-key "Find")
    "gfc" '(magit-show-commit :which-key "show commit")
    "gff" '(magit-find-file :which-key "file")
    "gfg" '(magit-find-git-config-file :which-key "git config file")
    "gfr" '(magit-list-repositories :which-key "repository")
    "gfs" '(magit-list-submodules) :which-key "submodule"))

;; Projectile
(use-package projectile
  :general
  (my-leader-def
    "p"  '(:ignore t :which-key "Projects")
    "pf" '(projectile-find-file :which-key "find file")
    "pF" '(projectile-find-other-file :which-key "find other file")
    "pd" '(projectile-remove-known-project :which-key "remove project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "pc" '(projectile-compile-project :which-key "compile project")
    "pk" '(projectile-kill-buffers :which-key "kill project buffers")
    "pr" '(projectile-recentf :which-key "find recent project")
    "ps" '(projectile-save-project-buffers :which-key "save project buffer"))
  :config
  (projectile-mode +1))

(use-package avy
  :general
  (my-leader-def
    "s" '(:ignore t :which-key "Search")
    "sf" '(avy-goto-char :which-key "char")
    "ss" '(avy-goto-char-2 :which-key "2-chars")
    "sl" '(avy-goto-line :which-key "line")
    "sw" '(avy-goto-word-1 :which-key "start of word")
    "so" '(avy-goto-heading-timer :which-key "org-heading")))
;; "sc" ('avy-org-refile-as-child :which-key "refile as child")))

(use-package company
  :hook (prog-mode . company-mode)
  :general
  (company-active-map "C-w" nil) ;; don't override evil C-w
  (general-imap
    "C-j" 'company-complete)) ;; manual completion with C-n

(use-package lsp-mode
  :general
  (general-nmap "gr" 'lsp-rename)
  :hook (prog-mode . lsp-mode))


(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :general
  (my-leader-def
    "fe" '(flycheck-list-errors :which-key "list errors"))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Color parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Theme
(use-package gruvbox-theme :ensure t)

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
