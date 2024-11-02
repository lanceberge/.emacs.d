;;; -*- lexical-binding: t -*-
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t))

(use-package csv-mode
  :hook (csv-mode . csv-align-mode))

(use-package json-mode
  :defer t)

(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

(use-package go-mode
  :defer t
  :config
  (setq-local use-tabs-mode t))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-basic-offset 2))

(use-package markdown-mode
  :ensure nil
  :custom
  (markdown-fontify-code-blocks-natively t)
  :general
  ('normal 'markdown-mode-map
           "RET" #'markdown-follow-thing-at-point))

(use-package markdown-toc ; create a table of contents
  :general
  ('markdown-mode-map
   :prefix "C-c"
   "t" #'markdown-toc-generate-toc))

(use-package text-mode
  :defer t
  :ensure nil
  :custom
  (text-mode-ispell-word-completion nil))

(use-package yaml-mode
  :mode ("\\.gotmpl\\'" . yaml-mode))

(use-package dockerfile-mode
  :defer t)
