;;; -*- lexical-binding: t -*-
(use-package python-mode
  :hook
  (python-mode . lsp-deferred)
  :defer t)
