;;; -*- lexical-binding: t -*-
(use-package web-mode
  :mode ("\\.html\\'"   . web-mode)
  :mode ("\\.css\\'"    . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-engines-alist
   '(("svelte" . "\\.svelte\\'")))
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  )

(use-package html-mode
  :ensure nil
  :mode ("\\.html\\'" . html-mode))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (svelte-mode . emmet-mode)
  :general
  ('insert emmet-mode-keymap
           "TAB" #'emmet-expand-line))

(use-package svelte-mode
  :defer t
  :custom
  (svelte-basic-offset 2))
