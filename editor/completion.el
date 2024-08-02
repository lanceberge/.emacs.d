;;; -*- lexical-binding: t -*-
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  (my-leader-def
    ;; TODO consult-project-buffer
    "SPC" #'(consult-buffer :which-key "find buffer")
    "fr"  #'(consult-recent-file :which-key "find recent file")
    "fs"  #'(consult-ripgrep :which-key "ripgrep"))
  ('normal
   "/" #'(consult-line :which-key "line"))

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package ivy ; narrowing framework
  :defer 0.1
  :hook (pre-command . ivy-mode)
  :general
  ('(normal insert) ivy-minibuffer-map
   ";"   #'exit-minibuffer
   "S-SPC" (lambda () (interactive) (insert " "))
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line)
  ('(normal insert) minibuffer-local-mode-map
   ";" #'exit-minibuffer)

  ('(normal insert) minibuffer-inactive-mode-map
   ";" #'ivy-done)
  :custom
  (ivy-initial-inputs-alist nil) ; no initial ^, let flx do all the sorting work
  :config
  (setq ivy-re-builders-alist '((swiper-isearch        . ivy--regex-plus)
                                (counsel-rg            . ivy--regex-plus)
                                (t                     . ivy--regex-fuzzy)))
  (evil-collection-init 'minibuffer)
  (evil-collection-init 'ivy)
  )

(use-package counsel ; ivy support for many functions
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :general
  (my-leader-def
    "."       #'(counsel-find-file :which-key "file in directory")
    "SPC"     #'(ivy-switch-buffer :which-key "switch buffer")
    "fj"      #'(counsel-imenu     :which-key "imenu")
    "gff"     #'(counsel-git       :which-key "git files")
    "ps"      #'(counsel-git-grep  :which-key "git grep")
    "f SPC f" #'(counsel-file-jump :which-key "file")
    "fih"       (lambda () (interactive) (counsel-file-jump "" "~"))
    "fis"       (lambda () (interactive) (counsel-file-jump "" "~/school"))
    "fic"       (lambda () (interactive) (counsel-file-jump "" "~/code"))
    "fio"       (lambda () (interactive) (counsel-file-jump "" "~/org"))
    "fie"       (lambda () (interactive) (counsel-file-jump "" "~/.emacs.d"))
    "fid"       (lambda () (interactive) (counsel-file-jump "" "~/Downloads"))
    "fd"      #'(counsel-dired     :which-key "directory")
    "pr"      #'(counsel-rg        :which-key "ripgrep")

    "ofo" (lambda () (interactive)
            (tab-bar-switch-to-tab "org")
            (counsel-find-file "~/org") :which-key "org")

    "ofs" (lambda () (interactive)
            (counsel-find-file "~/school/spring2022") :which-key "school"))
  :config
  (which-key-add-key-based-replacements
    "SPC fih" "find in ~"
    "SPC fis" "find in school"
    "SPC fin" "find in notes"
    "SPC fic" "find in code"
    "SPC fio" "find in org"
    "SPC fie" "find in dotemacs"
    "SPC fid" "find in downloads")
  (counsel-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package flx :defer t)

(use-package embark
  :general
  (my-localleader-def
    "a" #'embark-act
    "e" #'embark-export)
  )

(use-package vertico
  :init
  (vertico-mode)
  )

(use-package embark-consult
  :disabled t)

(use-package orderless
  :disabled t)

(use-package wgrep
  :disabled t)

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

(use-package company-flx ; fuzzy sorting for company completion options with company-capf
  :hook (company-mode . company-flx-mode))
