;;; -*- lexical-binding: t -*-
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  ('projectile-command-map
   "SPC p" #'(+projectile-switch-and-rg :which-key "switch proj and ripgrep")
   "s"     #'(consult-ripgrep           :which-key "ripgrep"))
  (my-leader-def
    ;; TODO consult-project-buffer
    "SPC" #'(consult-buffer      :which-key "find buffer")
    "."   #'(find-file           :which-key "find file")
    "fr"  #'(consult-recent-file :which-key "find recent file")
    "fj"  #'(consult-imenu       :which-key "imenu")
    "fo"  #'(consult-outline     :which-key "outline")
    "fs"  #'(consult-ripgrep     :which-key "ripgrep"))
  ('normal
   "/" #'(consult-line :which-key "line"))

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package vertico
  :defer 0.1
  :init
  (evil-collection-init 'minibuffer)
  (vertico-mode)
  :general
  ('(normal insert) '(vertico-map minibuffer-inactive-mode-map)
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-u" #'vertico-scroll-down
   "C-d" #'vertico-scroll-up
   ";"   #'vertico-exit))

(use-package marginalia
  :defer 0.2
  :config
  (marginalia-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :defer 0.3
  :general
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

(use-package company ; autocomplete
  :defer 0.1
  :custom
  (company-idle-delay 0.01)
  (company-require-match 'never)
  (company-show-numbers t)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-downcase nil)
  (company-tooltip-offset-display nil)
  (company-dabbrev-minimum-length 3)
  (company-minimum-prefix-length 2)
  :general
  ('company-active-map "C-w" nil ; don't override evil C-w
                       "C-j"      #'company-select-next-or-abort
                       "C-k"      #'company-select-previous-or-abort
                       "<tab>"    #'yas-expand ; don't interfere with yasnippet
                       "C-0"        (lambda () (interactive) (company-complete-number 10))
                       "RET"      #'newline
                       "<return>" #'newline
                       ";"        #'company-complete-selection) ; choose a completion with ; instead of tab
  :config
  ;; TODO configure this in prog-mode and text-mode use-package
  (defun prog-mode-company-backends ()
    (setq-local company-backends
                '((company-capf company-files company-dabbrev-code company-yasnippet))))

  (add-hook 'prog-mode-hook 'prog-mode-company-backends)

  (defun text-mode-company-backends ()
    (setq-local company-backends
                '((company-files company-dabbrev))))

  (add-hook 'text-mode-hook 'text-mode-company-backends)
  ;; complete suggestion based on the number
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (kbd (format "C-%d" x))
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 1 9)))
  (global-company-mode))

(use-package flx :defer t)

(use-package company-flx ; fuzzy sorting for company completion options with company-capf
  :hook (company-mode . company-flx-mode))
