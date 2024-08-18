;;; -*- lexical-binding: t -*-
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-basic-offset 2))

(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t)
  :config
  (require 'dap-node))

(use-package json-mode
  :defer t)
