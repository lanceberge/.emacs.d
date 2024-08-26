;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :straight (:type built-in)
  :mode ("\\.cu\\'" . c-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux"))

(use-package gdb-mi
  :straight (:type built-in)
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))
