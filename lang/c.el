;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux"))

(use-package gdb-mi
  :ensure nil
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))
