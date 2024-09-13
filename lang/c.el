;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :init
  (require 'dap-lldb)
  :hook
  (c++-mode . +cpp-mode)
  (c++-ts-mode . +cpp-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  (dap-lldb-debug-program "/opt/homebrew/opt/llvm/bin/lldb-dap")
  :config
  (defun +cpp-mode ()
    (setq-local tab-width 4
                dap-debug-template-configurations nil)

    (dap-register-debug-template
     "lldb"
     (list :type "lldb-vscode"
           :request "launch"
           :name "lldb"
           :MIMode "lldb-dap"
           :program "${workspaceFolder}/average_contiguous_subarray"
           :cwd "${workspaceFolder}"))))

(use-package gdb-mi
  :ensure nil
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))
