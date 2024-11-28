;;; -*- lexical-binding: t -*-
(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-script-padding 2)
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.vue\\'" . vue-mode)
  :mode ("\\.blade.php\\'" . web-mode)
  :config
  (setf (alist-get "vue" web-mode-engines-auto-pairs) '(("{{ " . " "))))

(use-package html-mode
  :ensure nil
  :mode ("\\.html\\'" . html-mode))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (svelte-mode . emmet-mode)
  :general
  ('insert 'emmet-mode-keymap
           "C-f" #'emmet-expand-line))

(use-package svelte-mode
  :defer t
  :mode ("\\.svelte\\'" . svelte-mode)
  :custom
  (svelte-basic-offset 2))
