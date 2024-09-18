;;; -*- lexical-binding: t -*-
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t))

(use-package html-mode
  :ensure nil
  :mode ("\\.html\\'" . html-mode))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (svelte-mode . emmet-mode))

(use-package svelte-mode
  :defer t
  :custom
  (svelte-basic-offset 2))

(use-package hippie-exp
  :ensure nil
  :defer t
  :general
  ('insert
   "TAB" #'hippie-expand)
  :config
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))
