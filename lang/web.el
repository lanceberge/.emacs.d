;;; -*- lexical-binding: t -*-
(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-script-padding 2)
  (web-mode-engines-auto-pairs nil)
  (web-mode-engine-open-delimiter-regexps nil)
  (web-mode-engines nil)
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.vue\\'" . vue-mode))

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
