;;; -*- lexical-binding: t -*-
(use-package elixir-mode
  :bind
  (:map elixir-mode-map
        ([remap apheleia-format-buffer] . #'+elixir-format-buffer)))

(use-package elixir-ts-mode
  :bind
  (:map elixir-ts-mode-map
        ([remap apheleia-format-buffer] . #'+elixir-format-buffer)))

(use-package elixir-web
  :after modal
  :ensure (:type file :main "~/.emacs.d/lisp/elixir-web.el")
  :hook
  ((elixir-ts-mode elixir-mode) . +elixir-web-maybe-enable)
  ((elixir-ts-mode elixir-mode) . +elixir--maybe-setup-new-file)
  ((elixir-ts-mode elixir-mode) . #'+elixir--maybe-setup-new-file)
  :config
  (+modal-bind +insert-mode-map elixir-web-mode-hook
               (">" . #'+elixir-web-maybe-close-tag)
               ([remap newline] . #'+elixir-web-newline)
               ([remap +comment-dwim] . #'+elixir-web-comment)))

(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t))

(use-package csv-mode
  :ensure nil)

(use-package json-mode
  :ensure nil)

(use-package go-mode
  :hook
  (go-mode . (lambda () (setq-local use-tabs-mode t))))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-basic-offset 2)
  :hook
  (js-mode . (lambda () (setq-local tab-width 2))))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package text-mode
  :ensure nil
  :custom
  (text-mode-ispell-word-completion nil))

(use-package yaml-mode)

;;;###autoload
(define-derived-mode gotmpl-mode yaml-mode "GoTmpl"
  :syntax-table yaml-mode-syntax-table)

(add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . gotmpl-mode))

(use-package yaml-imenu
  :if (executable-find "ruby")
  :init
  (yaml-imenu-enable)
  (remove-hook 'yaml-ts-mode-hook 'yaml-set-imenu-generic-expression)
  (add-hook 'yaml-ts-mode-hook 'yaml-imenu-activate t)
  (add-hook 'gotmpl-mode-hook 'yaml-imenu-activate t))

(use-package dockerfile-mode)

(use-package terraform-mode)

(use-package sh-mode
  :ensure nil)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package nxml-mode
  :ensure nil
  :custom
  (nxml-auto-insert-xml-declaration-flag nil)
  (nxml-validate-on-save nil)
  :bind
  (:map nxml-mode-map
        (">" . #'+nxml-close-tag)))

;;;###autoload
(defun +nxml-close-tag ()
  (interactive)
  (insert ">")
  (condition-case nil
      (progn
        (nxml-finish-element)
        (+open-above))
    (error nil)))

(use-package rust-mode)

(use-package nix-mode)

(use-package java-mode
  :ensure nil
  :hook
  (java-mode . (lambda ()
                 (setq-local indent-tabs-mode nil)))
  :config
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk-amd64"))

(use-package typst-ts-mode
  :ensure (:host codeberg :repo "meow_king/typst-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist
               '(typst "https://github.com/Ziqi-Yang/tree-sitter-typst")))

(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode))
