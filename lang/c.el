;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :hook
  (c++-mode . +cpp-mode)
  (c++-ts-mode . +cpp-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun +cpp-mode ()
    (setq-local tab-width 2)))

(use-package gdb-mi
  :ensure nil
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))
