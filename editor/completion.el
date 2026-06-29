;;; -*- lexical-binding: t -*-
(use-package corfu
  :defer 0.7
  :hook
  ((prog-mode text-mode agent-shell-mode eshell-mode) . corfu-mode)
  (+normal-mode . corfu-quit)
  (corfu-mode . corfu-indexed-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-delay 0.1)
  (corfu-separator ?\s)
  (corfu-on-exact-match 'show)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect 'first)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  (completion-ignore-case t)
  :bind
  (:map corfu-map
        ("C-y" . #'corfu-insert)
        ("<tab>" . #'yas-expand))
  :config
  (keymap-unset corfu-map "RET")
  (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
  (keymap-unset corfu-map "<remap> <move-end-of-line>")
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
                            (call-interactively #'corfu-complete))))))

;;;###autoload
(defun +corfu-minibuffer-mode ()
  "Enable `corfu-mode' in minibuffers where no completion UI is active."
  (when (and (not (bound-and-true-p vertico--input))
             (not (bound-and-true-p mct--active))
             (not (eq (current-local-map) read-passwd-map)))
    (corfu-mode)))

(add-hook 'minibuffer-setup-hook #'+corfu-minibuffer-mode 100)

(use-package orderless
  :after vertico
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package cape
  :hook
  (minibuffer-setup . +minibuffer-completion-mode)
  (org-mode . +org-completion-mode)
  (emacs-lisp-mode . +elisp-completion-mode)
  (eglot-managed-mode . +eglot-completion-mode)
  :custom
  (cape-file-directory-must-exist nil)
  (cape-dict-file "/usr/share/cracklib/cracklib-small")
  :init
  (setq-default completion-at-point-functions
                '(cape-file cape-dabbrev)))

;;;###autoload
(defun +elisp-completion-mode ()
  (setq-local completion-at-point-functions
              (list
               #'elisp-completion-at-point
               #'cape-dabbrev
               t)))

;;;###autoload
(defun +org-completion-mode ()
  (dolist (backend '(cape-dict cape-dabbrev cape-elisp-block))
    (add-hook 'completion-at-point-functions backend nil t)))

;;;###autoload
(defun +minibuffer-completion-mode ()
  (dolist (backend '(cape-history))
    (add-hook 'completion-at-point-functions backend nil t))
  (setq-local corfu-auto-prefix 3))

(use-package yasnippet-capf
  :ensure (:host github :repo "elken/yasnippet-capf")
  :hook
  (yas-minor-mode . +yas-completion-mode))

;;;###autoload
(defun +yas-completion-mode ()
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

;;;###autoload
(defun +eglot-completion-mode ()
  (setq-local completion-at-point-functions
              (list
               (cape-capf-super #'eglot-completion-at-point #'yasnippet-capf)
               #'eglot-completion-at-point
               t)))

(use-package esh-completion
  :ensure (:type file :main "~/.emacs.d/lisp/esh-completion.el")
  :hook (eshell-mode . +esh-completion-mode)
  :custom
  (+esh-completion-script (expand-file-name "list_completions.sh" user-emacs-directory)))

;;;###autoload
(use-package completion-preview
  :disabled t
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
