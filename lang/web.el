;;; -*- lexical-binding: t -*-
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.vue\\'" . web-mode)
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
  :mode ("\\.svelte\\'" . svelte-mode)
  :custom
  (svelte-basic-offset 2))
