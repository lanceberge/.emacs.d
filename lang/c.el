;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :init
  (dap-cpptools-setup)
  (require 'dap-cpptools)
  :hook
  (c++-mode . +cpp-mode)
  (c++-ts-mode . +cpp-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun +cpp-mode ()
    (setq-local tab-width 4))

  ;; TODO
  (dap-register-debug-template
   "cpptools::Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run Configuration"
         :MIMode "gdb"
         :program "${workspaceFolder}/average_contiguous_subarray"
         :cwd "${workspaceFolder}")))

(use-package gdb-mi
  :ensure nil
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))
