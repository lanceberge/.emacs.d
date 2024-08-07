;;; -*- lexical-binding: t -*-
(use-package web-mode
  :mode ("\\.html\\'"   . web-mode)
  :mode ("\\.svelte\\'" . web-mode)
  :mode ("\\.css\\'"    . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t))

(use-package html-mode
  :straight (:type built-in)
  :mode ("\\.html\\'" . html-mode))

(use-package emmet-mode
  :hook (web-mode . emmet-mode)
  :general
  ('insert emmet-mode-keymap
           "TAB" #'emmet-expand-line))
