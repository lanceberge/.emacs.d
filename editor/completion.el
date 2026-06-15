;;; -*- lexical-binding: t -*-
(use-package corfu
  :defer 1.4
  :hook ((prog-mode text-mode) . corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t) ; manual only — trigger with M-/ (see +insert-mode-map below)
  (corfu-delay 0.2)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  (completion-ignore-case t)
  :bind
  (:map corfu-map
        ("C-k" . #'corfu-previous)
        ("C-j" . #'corfu-next)
        ("M-j" . #'corfu-next)
        ("M-k" . #'corfu-previous)
        ("C-y" . #'corfu-insert)
        ("RET" . #'newline)
        ("<tab>" . #'yas-expand))
  :config
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local completion-styles '(flex basic)
                          completion-category-overrides nil
                          completion-category-defaults nil)))
  (cl-loop for idx from 0 to 9
           do
           (define-key corfu-map (kbd (format "M-%d" idx))
                       `(lambda () (interactive)
                          (let ((corfu--index ,idx))
                            (call-interactively #'corfu-complete)))))
  ;; (global-corfu-mode)
  (corfu-indexed-mode))

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
  ;; (org-mode . +org-completion)
  :custom
  (cape-file-directory-must-exist nil)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;;;###autoload
(defun +org-completion ()
  (dolist (backend '(#'org-roam-complete-everywhere
                     #'org-roam-complete-link-at-point))
    (add-to-list 'completion-at-point-functions backend)))

;;;###autoload
(defun +cape-minibuffer-mode ()
  (dolist (backend '(cape-history))
    (add-to-list 'completion-at-point-functions backend))
  (setq-local corfu-auto-prefix 3))

;;;###autoload
(defun +cape-text-mode ()
  (setq-local corfu-auto-prefix 4))

(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 2)
  (completion-preview-idle-delay 0.3)
  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . nil)
        ("<tab>" . nil)
        ("C-y" . #'completion-preview-insert)))
