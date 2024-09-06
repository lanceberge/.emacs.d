;;; -*- lexical-binding: t -*-
(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :custom
  (lsp-pyls-server-command "~/miniconda3/bin/pylsp")
  ;; https://github.com/emacs-lsp/dap-mode/issues/306
  (dap-python-debugger 'debugpy)
  :init
  (require 'dap-python))

(use-package conda
  :hook
  (python-mode . conda-env-autoactivate-mode)
  (python-mode . (lambda ()
                   (when (bound-and-true-p conda-project-env-path)
                     (conda-env-activate-for-buffer)))))
