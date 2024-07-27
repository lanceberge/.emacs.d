;;; -*- lexical-binding: t -*-
(use-package swiper ; ivy for searching through buffers
  :custom
  (swiper-use-visual-line nil)
  (swiper-use-visual-line-p (lambda (a) nil))
  :general
  (my-leader-def
    "/"  #'swiper-isearch
    "?"  #'swiper-isearch-backward
    "fb" #'(swiper-multi :which-key "swiper in buffer")
    "fB" #'(swiper-all   :which-key "swiper in all buffers")))

(use-package ivy ; narrowing framework
  :defer 0.1
  :hook (pre-command . ivy-mode)
  :general
  ('(normal insert) ivy-minibuffer-map
   ";"   #'exit-minibuffer
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line)

  ('normal ivy-minibuffer-map
           "q" #'minibuffer-keyboard-quit)

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
  (evil-collection-init 'ivy))

(use-package counsel ; ivy support for many functions
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :general
  (my-leader-def
    "."       #'(counsel-find-file :which-key "file in directory")
    "SPC"     #'(ivy-switch-buffer :which-key "switch buffer")
    "fr"      #'(counsel-recentf   :which-key "find recent files")
    "fj"      #'(counsel-imenu     :which-key "imenu")
    "gff"     #'(counsel-git       :which-key "git files")
    "ps"      #'(counsel-git-grep  :which-key "git grep")
    "f SPC f" #'(counsel-file-jump :which-key "file")
    "ff"      #'(counsel-fzf       :which-key "fzf")
    "fih"       (lambda () (interactive) (counsel-file-jump "" "~"))
    "fis"       (lambda () (interactive) (counsel-file-jump "" "~/school"))
    "fic"       (lambda () (interactive) (counsel-file-jump "" "~/code"))
    "fio"       (lambda () (interactive) (counsel-file-jump "" "~/org"))
    "fie"       (lambda () (interactive) (counsel-file-jump "" "~/.emacs.d"))
    "fid"       (lambda () (interactive) (counsel-file-jump "" "~/Downloads"))
    "fd"      #'(counsel-dired     :which-key "directory")
    "p SPC s" #'(counsel-rg        :which-key "ripgrep")

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

(use-package flx :defer t)

(use-package yasnippet ; snippets
  :defer 0.2
  :defer-incrementally (eldoc easymenu help-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets" "~/org/snippets"))
  :general
  (my-leader-def
    "si" #'(yas-insert-snippet  :which-key "insert")
    "sn" #'(yas-new-snippet     :which-key "new")
    "sl" #'(yas-describe-tables :which-key "list")
    "sr" #'(yas-reload-all      :which-key "reload"))
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
                       "<tab>"    #'company-complete-selection
                       "0"          (lambda () (interactive) (company-complete-number 10))
                       "RET"      #'newline
                       "<return>" #'newline
                       ";"        #'company-complete-selection) ; choose a completion with ; instead of tab
  :config
  ;; TODO configure this in prog-mode and text-mode use-package
  (defun prog-mode-company-backends ()
    (setq-local company-backends
                '((company-capf company-files company-dabbrev-code))))

  (add-hook 'prog-mode-hook 'prog-mode-company-backends)

  (defun text-mode-company-backends ()
    (setq-local company-backends
                '((company-files company-dabbrev))))

  (add-hook 'text-mode-hook 'text-mode-company-backends)
  ;; complete suggestion based on the number
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 1 9)))
  (global-company-mode))

(use-package company-flx ; fuzzy sorting for company completion options with company-capf
  :hook (company-mode . company-flx-mode))

(use-package amx ; show recently used commands
  :hook (pre-command . amx-mode)
  :custom
  (amx-save-file (expand-file-name "amx-history" my/etc-dir))
  (amx-history-length 50))
