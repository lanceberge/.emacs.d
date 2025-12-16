;;; -*- lexical-binding: t -*-
(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-script-padding 2)
  (web-mode-auto-close-style 2)
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.vue\\'" . vue-mode)
  :mode ("\\.heex\\'" . web-mode)
  :mode ("\\.blade.php\\'" . web-mode)
  :config
  (setf (alist-get "vue" web-mode-engines-auto-pairs) '(("{{ " . " ")))
  (setf (alist-get "blade" web-mode-engines-auto-pairs) '(("{{ " . " ")))
  (setq web-mode-engines-alist
        '(("elixir" . "\\.heex\\'"))))

(use-package html-mode
  :ensure nil
  :mode ("\\.html\\'" . html-mode))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (svelte-mode . emmet-mode)
  (elixir-web-mode . emmet-mode)
  :bind
  (:map emmet-mode-keymap
        ("C-t" . #'emmet-expand-line)))

(use-package svelte-mode
  :mode ("\\.svelte\\'" . svelte-mode)
  :custom
  (svelte-basic-offset 2))
