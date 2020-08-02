(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-enable-at-startup nil
      load-prefer-newer t)
(package-initialize)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(garbage-collect)

; ;; Initial Settings
; (setq user-full-name "Lance Bergeron"
;       user-mail-address "bergeron.lance6@gmail.com"
;       inhibit-startup-screen t)

; (menu-bar-mode -1) ; no menu bar
; (toggle-scroll-bar -1) ; no scroll bar
; (tool-bar-mode -1) ; no tool bar
; (set-frame-font "DejaVu Sans Mono") ; font
; (set-face-attribute 'default nil :height 110) ; font size
; (menu-bar-display-line-numbers-mode 'visual) ; relative line numbers
; (global-display-line-numbers-mode 1) ; always show line numbers

; ;; Keybindings
; (use-package which-key :ensure t
;   :config
;   (which-key-mode))

; (use-package general :ensure t
;   :config
;   (general-create-definer my-leader-def
;     :states '(normal visual insert emacs)
;     :prefix "SPC"
;     :non-normal-prefix "C-SPC")

;   (general-evil-setup)
;   (my-leader-def
;     "f"  '(:ignore t :which-key "Find")
;     "fm" '(general-describe-keybindings :which-key "list keybindings")
;     "q"  '(save-buffers-kill-emacs :which-key "save buffers & quit emacs")
;     "f." '(dired-jump :which-key "open dired")
;     "fd" '(dired :which-key "navigate to a directory")
;     "oa" '(org-agenda :which-key "org agenda")))

; ;; Evil
; (use-package evil :ensure t
;   :init
;   (setq evil-want-C-u-scroll t
; 	evil-want-Y-yank-to-eol t
; 	evil-split-window-below t
; 	evil-vsplit-window-right t
; 	evil-search-wrap t
; 	evil-want-keybinding nil)
;   :general
;   (evil-ex-completion-map ";" 'exit-minibuffer)
;   ('(normal visual motion)
;    ";" 'evil-ex
;    ":" 'evil-repeat-find-char
;    "H" "^"
;    "L" "$")
;   (general-nmap
;     ;; Vim-like Macros
;     "SPC =" "mzgg=G`z"
;     ;; Remaps
;     "gm" 'evil-execute-macro
;     "]b" '(evil-next-buffer :which-key "next buffer")
;     "[b" '(evil-prev-buffer :which-key "previous buffer"))
;   (my-leader-def
;     "h" (general-simulate-key "C-h")
;     ;; Windows
;     "w"  '(:ignore t :which-key "Windows")
;     "w" (general-simulate-key "C-w")
;     ;; Buffers
;     "b"  '(:ignore t :which-key "Buffers")
;     "bs" '(evil-write :which-key "write file")
;     "bd" '(evil-delete-buffer :which-key "delete buffer")
;     "bl" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
;     "bS" '(evil-write-all :which-key "write all buffers"))
;   :config
;   (define-key evil-window-map "d" 'evil-quit)
;   (define-key evil-window-map "q" 'evil-save-modified-and-close)
;   (evil-mode 1))

; ;; 2 character searches with s (ala vim-sneak)
; (use-package evil-snipe :ensure t
;   :hook ((prog-mode text-mode) . evil-snipe-local-mode)
;   :init
;   (setq evil-snipe-smart-case t))

; ;; Evil everywhere
; (use-package evil-collection :ensure t
;   :config
;   (evil-collection-init))

; ;; s as an operator for surrounding
; (use-package evil-surround :ensure t
;   :hook ((prog-mode text-mode) . evil-surround-mode))

; ;; gc as an operator to comment
; (use-package evil-commentary :ensure t
;   :hook (prog-mode . evil-commentary-mode))

; ;; jk to leave insert mode
; (use-package evil-escape :ensure t
;   :hook ((prog-mode text-mode) . evil-escape-mode)
;   :config
;   (setq evil-escape-key-sequence "jk"
; 	evil-escape-delay 0.25))

; ;; gl as an operator to left-align, gL to right-align
; (use-package evil-lion :ensure t
;   :hook ((prog-mode text-mode) . evil-lion-mode))

; ;; Persistent Undos
; (use-package undo-tree :ensure t
;   :hook ((prog-mode text-mode) . undo-tree-mode)
;   :init
;   (setq undo-limit 10000
; 	undo-tree-auto-save-history t))

; ;; z - prefixed folding options like vim
; (use-package evil-vimish-fold :ensure t
;   :hook ((prog-mode text-mode) . evil-vimish-fold-mode))

; (use-package ivy :ensure t
;   :config
;   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
;   (ivy-mode)
;   (use-package counsel :ensure t
;     :general
;     (my-leader-def
;       "."   '(counsel-find-file :which-key "find file")
;       "fb"  '(ivy-switch-buffer :which-key "switch buffer")
;       "fr"  '(counsel-recentf :which-key "find recent files")
;       "fl"  '(counsel-grep-or-swiper :which-key "find line"))
;     ;; "ff" '(counsel-locate) :which-key)

;     :config
;     (counsel-mode)
;     (use-package counsel-projectile :ensure t)
;     :general
;     (my-leader-def
;       "pp" '(counsel-projectile-switch-project :which-key "switch project")
;       "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer"))))
; ;; (use-package flx :ensure t))

; ;; Theme
; (use-package gruvbox-theme :ensure t)

; ;; Terminal Emulator
; (use-package vterm :ensure t
;   :general
;   (my-leader-def
;     "o"   '(:ignore t :which-key "Open")
;     "ot"  '(vterm :which-key "open vterm")
;     "ovt" '(vterm-other-window) :which-key "open vterm in vsplit")
;   :config
;   (setq vterm-kill-buffer-on-exit t))

; ;; Snippets
; (use-package yasnippet :ensure t
;   :hook (prog-mode . yas-minor-mode)
;   :general
;   (my-leader-def
;     "fs" 'yas-describe-tables)
;   :config
;   (use-package yasnippet-snippets :ensure t))

; ;; Color parentheses
; (use-package rainbow-delimiters :ensure t
;   :hook (prog-mode . rainbow-delimiters-mode))

; ;; Autopair parentheses
; ;; (use-package autopair :ensure t
; ;;   :config
; ;;   (autopair-global-mode))

; (use-package smartparens :ensure t
;   :hook (prog-mode . smartparens-mode)
;   :config
;   (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)) ; don't pair ' in elisp mode

; ;; Provides the restart-emacs function
; (use-package restart-emacs :ensure t
;   :general
;   (my-leader-def
;     "e"  '(:ignore t :which-key "Emacs Commands")
;     "er" '(restart-emacs :which-key "restart emacs"))
;   :config
;   (setq restart-emacs-restore-frames t)) ;; Restore frames on restart

; ;; Magit
; (use-package evil-magit :ensure t
;   :general
;   (my-leader-def
;     "g"   '(:ignore t :which-key "Magit")
;     "gs"  '(magit-status :which-key "status")
;     "gb"  '(magit-branch-checkout :which-key "checkout branch")
;     "gB"  '(magit-blame-addition :which-key "blame")
;     "gc"  '(magit-clone :which-key "clone")
;     "gd"  '(magit-file-delete :which-key "delete file")
;     "gF"  '(magit-fetch :which-key "fetch")
;     "gG"  '(magit-status-here :which-key "status here")
;     "gl"  '(magit-log :which-key "log")
;     "gS"  '(magit-stage-file :which-key "stage file")
;     "gU"  '(magit-unstage-file :which-key "unstage file")
;     "gn"  '(:ignore t :which-key "New")
;     "gnb" '(magit-branch-and-checkout :which-key "branch")
;     "gnc" '(magit-commit-create :which-key "commit")
;     "gnf" '(magit-commit-fixup :which-key "fixup commit")
;     "gnd" '(magit-init :which-key "init")
;     "gf"  '(:ignore t :which-key "Find")
;     "gfc" '(magit-show-commit :which-key "show commit")
;     "gff" '(magit-find-file :which-key "file")
;     "gfg" '(magit-find-git-config-file :which-key "git config file")
;     "gfr" '(magit-list-repositories :which-key "repository")
;     "gfs" '(magit-list-submodules) :which-key "submodule"))

; ;; Projectile
; (use-package projectile :ensure t
;   :general
;   (my-leader-def
;     "p"  '(:ignore t :which-key "Projects")
;     "pf" '(projectile-find-file :which-key "find file")
;     "pF" '(projectile-find-other-file :which-key "find other file")
;     "pd" '(projectile-remove-known-project :which-key "remove project")
;     "pa" '(projectile-add-known-project :which-key "add project")
;     "pc" '(projectile-compile-project :which-key "compile project")
;     "pk" '(projectile-kill-buffers :which-key "kill project buffers")
;     "pr" '(projectile-recentf :which-key "find recent project")
;     "ps" '(projectile-save-project-buffers :which-key "save project buffer"))
;   :config
;   (projectile-mode +1))

; (use-package avy :ensure t
;   :general
;   (my-leader-def
;     "s" '(:ignore t :which-key "Search")
;     "sf" '(avy-goto-char :which-key "char")
;     "ss" '(avy-goto-char-2 :which-key "2-chars")
;     "sl" '(avy-goto-line :which-key "line")
;     "sw" '(avy-goto-word-1 :which-key "start of word")
;     "so" '(avy-goto-heading-timer :which-key "org-heading")))
; ;; "sc" ('avy-org-refile-as-child :which-key "refile as child")))

; (use-package company :ensure t
;   :hook (prog-mode . company-mode)
;   :general
;   (company-active-map "C-w" nil) ;; don't override evil C-w
;   (general-imap
;     "C-j" 'company-complete)) ;; manual completion with C-n

; (use-package lsp-mode :ensure t
;   :general
;   (general-nmap "gr" 'lsp-rename)
;   :hook (prog-mode . lsp-mode))


; (use-package flycheck :ensure t
;   :hook (prog-mode . flycheck-mode)
;   :general
;   (my-leader-def
;     "fe" '(flycheck-list-errors :which-key "list errors"))
;   :config
;   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

; (use-package org-bullets :ensure t
;   :hook (org-mode . org-bullets-mode))

; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(package-selected-packages
;    (quote
;     (lsp-mode counsel-projectile yasnippet-snippets vterm use-package restart-emacs rainbow-delimiters projectile popup gruvbox-theme general evil-visual-mark-mode evil-vimish-fold evil-surround evil-snipe evil-magit evil-lion evil-escape evil-commentary evil-collection counsel autopair))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
