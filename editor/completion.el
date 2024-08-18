;;; -*- lexical-binding: t -*-
(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :general
  (
   [remap projectile-find-file] #'(consult-project-buffer :which-key "project buffer"))

  (my-leader-def
    "SPC"     #'(consult-buffer         :which-key "find buffer")
    "."       #'(find-file              :which-key "find file")
    "/"       #'(consult-line           :which-key "line")
    "fr"      #'(consult-recent-file    :which-key "find recent file")
    "fj"      #'(consult-imenu          :which-key "imenu")
    "fp"      #'(consult-project-buffer :which-key "project buffer")
    "f SPC j" #'(consult-imenu-multi    :which-key "imenu")
    "fm"      #'(consult-global-mark    :which-key "mark")
    "fp"      #'(consult-project-buffer :which-key "project buffer")
    "fo"      #'(consult-outline        :which-key "outline")
    "f."      #'(consult-find           :which-key "file")
    "fl"      #'(consult-goto-line      :which-key "outline")
    "fa"      #'(consult-org-agenda     :which-key "agenda")
    "fs"      #'(consult-ripgrep        :which-key "ripgrep")
    )

  ('org-agenda-mode-map
   [remap evil-search-forward] #'(consult-line :which-key "line"))

  ('isearch-mode-map
   "/" #'consult-line)
  :config
  (when IS-LINUX
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root))))
  )

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
   ";"   #'vertico-exit
   "M-h" #'vertico-directory-up
   "M-l" #'vertico-directory-enter
   "M-l" #'vertico-directory-enter
   "C-w" #'evil-delete-backward-word
   )
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

(use-package yasnippet ; snippets
  ;; https://joaotavora.github.io/yasnippet/snippet-development.html
  :defer 1.1
  :defer-incrementally (eldoc easymenu help-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets" "~/org/snippets"))
  :general
  ('yas-keymap
   "<tab>" #'yas-next-field)
  (my-leader-def
    "si" #'(yas-insert-snippet     :which-key "insert")
    "sn" #'(yas-new-snippet        :which-key "new")
    "sf" #'(yas-visit-snippet-file :which-key "find snippet")
    "sl" #'(yas-describe-tables    :which-key "list")
    "sr" #'(yas-reload-all         :which-key "reload"))
  :config
  ;; https://github.com/emacs-evil/evil/issues/254
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m)))))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas--remove-template-by-uuid (yas--table-get-create 'emacs-lisp-mode) "kill-buffer"))

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
   ";"   #'corfu-complete
   "<tab>" #'yas-expand
   )
  :config
  (global-corfu-mode)
  (advice-add 'evil-escape-func :after #'corfu-quit))

(use-package cape
  :after corfu
  :hook
  (text-mode        . +cape-text-mode)
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

  (dolist (backend '(cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))
