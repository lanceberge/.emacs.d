;;; -*- lexical-binding: t -*-
(use-package lsp-mode ; LSP
  :custom
  ;; Disable slow features
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-headerline-breadcrumb-enable nil)

  ;; Clean modeline
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)

  ;; Don't modify our code w/o permission
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-clients-typescript-server-args '("--stdio"))

  (lsp-auto-guess-root t)
  :general
  ('normal lsp-mode-map
           "gh" #'(lsp-describe-thing-at-point :which-key "view doc"))
  (my-localleader-def
    "h"  #'(lsp-describe-thing-at-point :which-key "view doc")
    "gr" #'(lsp-rename                  :which-key "rename with lsp")))

(use-package lsp-ui
  :general
  ('normal lsp-mode-map
           "gd" #'lsp-ui-peek-find-implementation))

(use-package flycheck ; code syntax checking
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay 0.25)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :general
  ('normal
   "[q" #'(flycheck-previous-error :which-key "previous error")
   "]q" #'(flycheck-next-error :which-key "next error"))

  (my-leader-def
    "fe" #'(+flycheck-list-errors :which-key "list errors"))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package xref
  :commands (xref-find-references xref-auto-jump-first-definition)
  :general
  ('normal xref--xref-buffer-mode-map
           ";" #'xref-goto-xref))

(use-package eldoc
  :general
  ('normal
   ;; TODO wrap in smaller buffer function
   "gh" #'(eldoc-print-current-symbol-info :which-key "view doc")))

(use-package project
  :straight (:type built-in)
  :general

  (my-leader-def
    "pp"      #'(+project-switch-and-find-file :which-key "switch project")
    "p SPC p" #'(+project-switch-and-rg        :which-key "switch project")
    "pf"      #'(project-find-file             :which-key "find file")
    "ps"      #'(consult-ripgrep               :which-key "ripgrep")))

(use-package projectile
  :disabled t
  :defer 0.2
  :custom
  (projectile-project-search-path '("~/code/" "~/src/" "~/org" ))
  :general
  (my-leader-def
    "p" #'projectile-command-map)
  ('projectile-command-map
   "p" #'(+projectile-switch-and-find-file :which-key "switch proj and find file")
   "s" #'(consult-ripgrep                  :which-key "ripgrep"))
  :config
  (projectile-mode +1))
