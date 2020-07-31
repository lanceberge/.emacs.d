(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-enable-at-startup nil)
(package-initialize)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Initial Settings
(setq user-full-name "Lance Bergeron"
      user-mail-address "bergeron.lance6@gmail.com"
      inhibit-startup-screen t)

(menu-bar-mode -1) ;; no menu bar
(toggle-scroll-bar -1) ;; no scroll bar
(tool-bar-mode -1) ;; no tool bar
(set-frame-font "DejaVu Sans Mono") ;; font
(set-face-attribute 'default nil :height 110) ;; font size
(menu-bar-display-line-numbers-mode 'visual) ;; relative line numbers
(global-display-line-numbers-mode 1) ;; always show line numbers


;; Keybindings
(use-package which-key :ensure t
  :config
  (which-key-mode))

(use-package general :ensure t
  :config
  (general-create-definer my-leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-evil-setup)
  (my-leader-def
    "f" '(:ignore t :which-key "Find")
    "fm" '(general-describe-keybindings :which-key "list keybindings")
    "q" '(save-buffers-kill-emacs :which-key "save buffers & quit emacs")
    "f." '(dired-jump :which-key "open dired")
    "fd" '(dired :which-key "navigate to a directory")))

;; Evil
(use-package evil :ensure t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-search-wrap t
	evil-want-keybinding nil)
  :general
  (evil-ex-completion-map ";" 'exit-minibuffer)
  (general-nmap
    ;; Vim-like Macros
    "SPC =" "mzgg=G`z"
    ;; Remaps
    "gm" 'evil-execute-macro
    ";" 'evil-ex
    ":" 'evil-repeat-find-char
    "H" "^"
    "L" "$"
    "]b" '(evil-next-buffer :which-key "next buffer")
    "[b" '(evil-prev-buffer :which-key "previous buffer"))
  (my-leader-def
    ;; Windows
    "w" '(:ignore t :which-key "Windows")
    "wq" '(evil-save-modified-and-close :which-key "save and close window")
    "wd" '(evil-quit :which-key "delete window")
    "wh" '(evil-window-left :which-key "naigate left")
    "wj" '(evil-window-down :which-key "navigate down")
    "wk" '(evil-window-up :which-key "navigate up")
    "wl" '(evil-window-right :which-key "navigate right")
    ;; Buffers
    "b" '(:ignore t :which-key "Buffers")
    "bs" '(evil-write :which-key "write file")
    "bd" '(evil-delete-buffer :which-key "delete buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
    "bS" '(evil-write-all :which-key "write all buffers"))
  :config
  (evil-mode 1)

  ;; Evil everywhere
  (use-package evil-collection :ensure t
    :config
    (evil-collection-init))

  ;; s as an operator for surrounding
  (use-package evil-surround :ensure t
    :config
    (global-evil-surround-mode 1))

  ;; gc as an operator to comment
  (use-package evil-commentary :ensure t
    :config
    (evil-commentary-mode))

  ;; jk to leave insert mode
  (use-package evil-escape :ensure t
    :config
    (evil-escape-mode)
    (setq evil-escape-key-sequence "jk"
	  evil-escape-delay 0.25))

  ;; gl as an operator to left-align, gL to right-align
  (use-package evil-lion :ensure t
    :config
    (evil-lion-mode))

  ;; z - prefixed folding options like vim
  (use-package evil-vimish-fold :ensure t
    :config
    (add-hook 'prog-mode-hook 'evil-vimish-fold-mode))

  ;; 2 character searches with s (ala vim-sneak)
  (use-package evil-snipe :ensure t
    :init
    (setq evil-snipe-smart-case t)
    :config
    (evil-snipe-mode +1)))

(use-package ivy :ensure t
  :config
  (ivy-mode)
  (use-package counsel :ensure t
    :general
    (my-leader-def
      "." '(counsel-find-file :which-key "find file")
      "fb" '(ivy-switch-buffer :which-key "switch buffer")
      "fr" '(counsel-recentf :which-key "find recent files")
      "fl" '(counsel-grep-or-swiper :which-key "find line"))
    ;; "ff" '(counsel-locate) :which-key)

    :config
    (counsel-mode)
    (use-package counsel-projectile :ensure t)
    :general
    (my-leader-def
      "pp" '(counsel-projectile-switch-project :which-key "switch project")
      "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer"))))

;; Theme
(use-package gruvbox-theme :ensure t)

;; Terminal Emulator
(use-package vterm :ensure t
  :general
  (my-leader-def
    "o" '(:ignore t :which-key "Open")
    "ot" '(vterm :which-key "open vterm")
    "ovt" '(vterm-other-window) :which-key "open vterm in vsplit")
  :config
  (setq vterm-kill-buffer-on-exit t))

;; Snippets
(use-package yasnippet :ensure t
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets :ensure t))

;; Color parentheses
(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Autopair parentheses
(use-package autopair :ensure t
  :config
  (autopair-global-mode))

;; Provides the restart-emacs function
(use-package restart-emacs :ensure t
  :general
  (my-leader-def
    "e" '(:ignore t :which-key "Emacs Commands")
    "er" '(restart-emacs :which-key "restart emacs"))
  :config
  ;; Restore frames on restart
  (setq restart-emacs-restore-frames t))

;; Magit
(use-package evil-magit :ensure t)

;; Projectile
(use-package projectile :ensure t
  :general
  (my-leader-def
    "p" '(:ignore t :which-key "Projects")
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

(use-package company :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-projectile yasnippet-snippets vterm use-package restart-emacs rainbow-delimiters projectile popup gruvbox-theme general evil-visual-mark-mode evil-vimish-fold evil-surround evil-snipe evil-magit evil-lion evil-escape evil-commentary evil-collection counsel autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

