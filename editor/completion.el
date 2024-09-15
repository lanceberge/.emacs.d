;;; -*- lexical-binding: t -*-
(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :general
  ([remap projectile-find-file] #'(consult-project-buffer :which-key "project buffer"))

  (my-leader-def
    "SPC" #'(consult-buffer :which-key "find buffer")
    "." #'(find-file :which-key "find file")
    "/" #'(consult-line :which-key "line")
    "fr" #'(consult-recent-file :which-key "find recent file")
    "fj" #'(consult-imenu :which-key "imenu")
    "fp" #'(consult-project-buffer :which-key "project buffer")
    "fm" #'(consult-global-mark :which-key "mark")
    "fp" (defun +find-package ()
           (interactive)
           (consult-ripgrep "~/.emacs.d/" "pack "))
    "fe" #'consult-flymake
    "fo" #'(consult-outline :which-key "outline")
    "f." #'(consult-find :which-key "file")
    "fl" #'(consult-goto-line :which-key "outline")
    "fa" #'(consult-org-agenda :which-key "agenda")
    "fs" #'(consult-ripgrep :which-key "ripgrep")
    "f SPC j" #'(consult-imenu-multi :which-key "imenu"))


  ('org-agenda-mode-map
   [remap evil-search-forward] #'(consult-line :which-key "line"))

  ('isearch-mode-map
   "/" #'consult-line))

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :general
  ('(normal insert) '(vertico-map minibuffer-inactive-mode-map)
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-u" #'vertico-scroll-down
   "C-d" #'vertico-scroll-up
   "M-j" #'vertico-next
   "M-k" #'vertico-previous
   "M-u" #'vertico-scroll-down
   "M-d" #'vertico-scroll-up
   ";" #'vertico-exit
   "M-h" #'vertico-directory-up
   "M-l" #'vertico-directory-enter
   "M-l" #'vertico-directory-enter
   "C-w" #'evil-delete-backward-word)
  :config
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
;;;###autoload
  (defun +auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions #'+auto-create-missing-dirs)

  (evil-collection-init 'minibuffer)
  (vertico-mode))

(use-package marginalia
  :defer 0.4
  :config
  (marginalia-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark-consult
  :after (consult embark))

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
  :general
  ('corfu-map
   "RET" #'newline
   "C-k" #'corfu-previous
   "M-j" #'corfu-next
   "M-k" #'corfu-previous
   "C-j" #'corfu-next
   ";" #'corfu-complete
   [remap evil-normal-state] #'corfu-quit
   "<tab>" #'yas-expand)

  ('insert 'corfu-map
           "C-k" #'corfu-previous)
  :config
  (global-corfu-mode)
  (advice-add 'evil-escape-func :after #'corfu-quit))

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
  (add-hook 'completion-at-point-functions #'cape-file))
