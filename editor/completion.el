;;; -*- lexical-binding: t -*-
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  (my-leader-def
    "SPC"     #'(consult-buffer         :which-key "find buffer")
    "."       #'(find-file              :which-key "find file")
    "/"       #'(consult-line           :which-key "line")
    "fr"      #'(consult-recent-file    :which-key "find recent file")
    "fj"      #'(consult-imenu          :which-key "imenu")
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
  ;; ('normal
  ;;  "/" #'(consult-line :which-key "line"))
  :config
  (when IS-LINUX
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root))))
  ;; This function is amazing
  (setq xref-show-xrefs-function #'consult-xref))

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  :init
  (evil-collection-init 'minibuffer)
  (vertico-mode)
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
   ";"   #'vertico-exit))

(use-package marginalia
  :defer 0.2
  :config
  (marginalia-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :defer 0.3
  :general
  ('(normal insert)
   "M-." #'embark-act
   "M-," #'embark-export)
  (my-localleader-def
    "a" #'embark-act
    "e" #'embark-export))

(use-package embark-consult
  :after (consult embark))

(use-package yasnippet ; snippets
  :defer 0.2
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
  ;; Latex-mode snippets in org
  (add-hook 'org-mode-hook (lambda ()
                             (yas-activate-extra-mode 'latex-mode)))
  (yas-global-mode 1))

(use-package corfu
  :defer 0.3
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
  :init
  (global-corfu-mode)
  :general
  ('(normal insert) 'corfu-map
   "RET"   #'newline
   "C-k"   #'corfu-previous
   "M-j"   #'corfu-next
   "M-k"   #'corfu-previous
   "C-j"   #'corfu-next
   ";"     #'corfu-complete
   "<tab>" #'yas-expand
   "<escape>" (lambda () (interactive)
                (corfu-quit)
                (evil-normal-state))
   )
  )

(use-package cape
  :after corfu
  :custom
  (cape-file-directory-must-exist nil)
  :init
  (defun text-mode-cape-backends ()
    (dolist (backend '(cape-dabbrev cape-dict))
      (add-to-list 'completion-at-point-functions backend)))

  (add-hook 'text-mode-hook 'text-mode-cape-backends)

  (dolist (backend '(cape-file))
    (add-to-list 'completion-at-point-functions backend)))
