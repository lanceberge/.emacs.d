;;; -*- lexical-binding: t -*-
(use-package corfu
  :defer 1.4
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect 'first)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  (completion-ignore-case t)
  :general
  ('corfu-map
   "RET" #'newline
   "C-k" #'corfu-previous
   "M-j" #'corfu-next
   "M-k" #'corfu-previous
   "C-j" #'corfu-next
   "C-y" #'corfu-complete
   "<tab>" #'yas-expand)

  ('insert 'corfu-map
           "C-y" #'corfu-complete
           "C-k" #'corfu-previous)
  :config
  (global-corfu-mode)
  (advice-add 'evil-escape-func :after #'corfu-quit))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package cape
  :after corfu
  :hook
  (text-mode . +cape-text-mode)
  (minibuffer-setup . +cape-minibuffer-mode)
  :custom
  (cape-file-directory-must-exist nil)
  :init
  (defun +cape-text-mode ()
    (setq-local corfu-auto-prefix 4))

  (defun +cape-minibuffer-mode ()
    (dolist (backend '(cape-history))
      (add-to-list 'completion-at-point-functions backend))
    (setq-local corfu-auto-prefix 3))

  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
