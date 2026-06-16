;;; -*- lexical-binding: t -*-
(use-package corfu
  :defer 1.4
  :hook
  ((prog-mode text-mode agent-shell-mode eshell-mode) . corfu-mode)
  (+normal-mode . corfu-quit)
  (corfu-mode . corfu-indexed-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
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
  :after corfu
  :hook
  (text-mode . +cape-text-mode)
  (minibuffer-setup . +cape-minibuffer-mode)
  (org-mode . +org-completion)
  :custom
  (cape-file-directory-must-exist nil)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;;;###autoload
(defun +org-completion ()
  (dolist (backend '(cape-elisp-block))
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
