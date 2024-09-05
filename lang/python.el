;;; -*- lexical-binding: t -*-
(use-package python-mode
  :defer t
  :custom
  (lsp-pyls-server-command "~/miniconda3/bin/pyls")
  ;; https://github.com/emacs-lsp/dap-mode/issues/306
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package conda
  :hook
  (python-mode . conda-env-autoactivate-mode)
  (python-mode . (lambda ()
                   (when (bound-and-true-p conda-project-env-path)
                     (conda-env-activate-for-buffer)))))
