;;; -*- lexical-binding: t -*-
(use-package lsp-mode ; LSP
  :defer 2.0
  :defer-incrementally (markdown-mode lsp-ui)
  :hook
  ((go-mode
    java-mode
    js2-mode
    python-mode
    svelte-mode
    typescript-ts-mode
    typescript-mode
    c++-mode
    c++-ts-mode) . lsp-deferred)

  (lsp-mode-hook . (lambda ()
                     (add-hook 'before-save-hook #'lsp-organize-imports)))
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
  (lsp-auto-execute-action nil)
  (lsp-enable-imenu t)

  (lsp-enable-dap-auto-configure t)

  (lsp-diagnostics-provider :flycheck)
  (lsp-completion-provider  :none) ;; Corfu
  (lsp-completion-enable-additional-text-edit t)
  :init
  (defun +lsp-mode-setup-completion ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  :hook
  (lsp-mode . +lsp-mode-setup-completion)
  :general
  ('(normal insert) 'lsp-mode-map
   "M-i" #'(lsp-execute-code-action :which-key "code action"))

  ('normal 'lsp-mode-map
           "K"  #'(lsp-describe-thing-at-point :which-key "find references")
           "ga" #'(lsp-execute-code-action     :which-key "code action")
           "gh" #'(lsp-describe-thing-at-point :which-key "view doc"))

  (my-localleader-def
    "h"  #'(lsp-describe-thing-at-point :which-key "view doc")
    "gr" #'(lsp-rename                  :which-key "rename with lsp"))

  ('(normal insert visual) 'lsp-mode-map
   [remap display-local-help]    #'lsp-describe-thing-at-point
   [remap xref-find-definitions] #'lsp-find-definition)
  :config
  ;; lsp-booster
  (when IS-MAC
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

(use-package dap-mode
  :defer-incrementally (hydra)
  :custom
  ;; configure what the ui shows by default
  ;; (dap-auto-configure-features '(sessions locals tooltip))
  (lsp-enable-dap-auto-configure nil)
  :general
  (my-localleader-def
    "db" #'(dap-breakpoint-toggle      :which-key "breakpoint")
    "dr" #'(dap-debug-recent           :which-key "debug recent")
    "ds" #'(dap-switch-stack-frame     :which-key "switch stack frame")
    "dh" #'(dap-hydra                  :which-key "hydra")
    "d,b" #'(dap-breakpoint-delete-all :which-key "breakpoint")
    "d,d" #'(dap-disconnect            :which-key "disconnect")
    "dd" (defun +dap-debug ()
           (interactive)
           (call-interactively #'dap-debug)
           (dap-hydra))
    "dl" (defun +dap-debug-last ()
           (interactive)
           (call-interactively #'dap-debug-last)
           (dap-hydra)))
  :config
  (dap-ui-mode 1))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (evil-lookup-func #'lsp-ui-doc-glance )
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point)
  :general
  ('normal 'lsp-mode-map
           "gd" #'lsp-ui-peek-find-implementation))

(use-package flycheck ; code syntax checking
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay 0.25)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :general
  ('normal
   "[oq" (defun +flycheck-off () (interactive)
                (flycheck-mode -1))
   "]oq" (defun +flycheck-ofn () (interactive)
                (flycheck-mode 1))
   "[q" #'(flycheck-previous-error :which-key "previous error")
   "]q" #'(flycheck-next-error :which-key "next error"))

  (my-leader-def
    "fe" #'(+flycheck-list-errors :which-key "list errors"))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package consult-flycheck
  :general
  (my-leader-def
    "fe" #'(consult-flycheck :which-key "outline")))

(use-package xref
  :commands (xref-find-references xref-auto-jump-first-definition)
  :custom
  (xref-prompt-for-identifier nil)
  :general
  ('normal xref--xref-buffer-mode-map
           ";" #'xref-goto-xref))

(use-package eldoc
  :ensure nil
  :preface
  ;; avoid loading of built-in eldoc, see https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229
  (when IS-LINUX
    (unload-feature 'eldoc t))
  :general
  ('normal
   "gh" #'(eldoc-print-current-symbol-info :which-key "view doc")))

(use-package project
  :commands (project-switch-project)
  :general
  (my-leader-def
    "pp" #'(+project-switch-and-find-file :which-key "switch project")
    "pg" #'(+project-switch-and-magit-status :which-key "switch project")
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "p SPC p" #'(+project-switch-and-rg :which-key "switch project")))
