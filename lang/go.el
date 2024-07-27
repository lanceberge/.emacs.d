(use-package go-mode
  :hook
  (go-mode . lsp-deferred)
  :defer t)
