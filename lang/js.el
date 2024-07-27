;;; -*- lexical-binding: t -*-
(use-package js2-mode
  :hook
  (js2-mode . lsp-deferred)
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-basic-offset 2))

(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t))

(use-package json-mode
  :defer t)
