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
  ('normal
   [remap evil-goto-definition] #'lsp-find-definition)
  (my-localleader-def
    "h"  #'(lsp-describe-thing-at-point :which-key "view doc")
    "gr" #'(lsp-rename                  :which-key "rename with lsp")))

(use-package lsp-ivy
  :defer t
  :general
  (my-localleader-def
    "t" (lambda () (interactive)
          (lsp-ivy-workspace-symbol 'symbol-at-point)
          :which-key "view usages")))

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
  :commands xref-find-references xref-auto-jump-first-definition)
