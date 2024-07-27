;;; -*- lexical-binding: t -*-
(use-package org
  :straight (:type built-in)
  :hook
  (org-tab-first . yas-expand)
  :defer-incrementally
  (calendar find-func format-spec org-macs org-compat
            org-faces org-entities org-list org-pcomplete org-src
            org-footnote org-macro ob org org-clock org-agenda
            org-capture evil-org flyspell)
  :custom
  ;; Directories
  (org-id-locations-file (expand-file-name ".org-id-locations" my/etc-dir))

  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "notes.org/" org-directory ))

  ;; General settings
  (org-list-allow-alphabetical t)
  (org-startup-folded t)
  (org-fontify-done-headline t)
  (org-M-RET-may-split-line nil)
  (org-return-follows-link t)

  ;; Latex exports
  (org-export-backends '(html latex md))
  (org-latex-listings 'minted) ; syntax-highlighted code blocks
  ;; (org-latex-packages-alist '(("margin=0.5in" "geometry" nil) (nil "minted" "color")))
  (org-latex-pdf-process ; required to use minted
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-minted-options '(("linenos" "true") ; line numbers in expored src blocks
                              ("frame" "lines")
                              ("style" "emacs")))
  :general
  (my-localleader-def
    :keymaps 'org-mode-map
    "t" #'org-set-tags-command)

  ('(normal insert) org-mode-map
   ;; TODOS with M-;, headlines with C-;, add shift to do those above
   "C-M-;"        #'+org/insert-subheading
   "C-;"          #'+org/insert-heading
   "C-:"          #'+org/insert-heading-above
   "M-;"          #'+org/insert-todo
   "M-:"          #'+org/insert-todo-above
   "M-<return>"   #'+org/insert-todo
   "M-S-<return>" #'+org/insert-todo-above
   "C-<return>"   #'+org/insert-heading
   "C-S-<return>"   #'+org/insert-heading-above

   ;; Vim keys > arrow keys
   "M-h"   #'org-metaleft
   "M-j"   #'org-metadown
   "M-k"   #'org-metaup
   "M-l"   #'org-metaright

   "M-H"   #'org-shiftleft
   "M-J"   #'org-shiftdown
   "M-K"   #'org-shiftup
   "M-L"   #'org-shiftright

   "C-M-h" #'org-shiftmetaleft
   "C-M-j" #'org-shiftmetadown
   "C-M-k" #'org-shiftmetaup
   "C-M-l" #'org-shiftmetaright

   "C-S-h" #'org-shiftcontrolleft
   "C-S-j" #'org-shiftcontroldown
   "C-S-k" #'org-shiftcontrolup
   "C-S-l" #'org-shiftcontrolright)

  ('(normal insert) :prefix "C-c"
   "e"  #'(org-latex-export-to-pdf     :which-key "export to pdf")
   ",v" #'(org-redisplay-inline-images :which-key "redisplay inline images")
   "v"  #'(org-toggle-inline-images    :which-key "toggle inline images")
   "t"  #'(org-todo                    :which-key "todo")
   "s"  #'(org-sort                    :which-key "sort")
   ",s" #'(org-schedule                :which-key "schedule")
   "d"  #'(org-deadline                :which-key "deadline")
   "q"  #'(org-set-tags-command        :which-key "add tags")
   "p"  #'(org-latex-preview           :which-key "preview latex")
   ",p" #'(org-set-property            :which-key "set property"))

  ('normal org-mode-map
           "zo" #'outline-show-subtree
           "zk" #'org-backward-element
           "zj" #'org-forward-element)

  ;; Vim keys calendar maps
  ('org-read-date-minibuffer-local-map
   ";" #'exit-minibuffer
   "M-h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
   "M-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
   "M-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
   "M-l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
   "M-H" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1)))
   "M-J" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1)))
   "M-K" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1)))
   "M-L" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
  :config
  (defun org-mode-company-backends ()
    (setq-local company-backends
                '((company-files company-capf))))

  (add-hook 'org-mode-hook 'org-mode-company-backends)

  (setq org-tag-alist '(("personal"      . ?p)
                        ("easy tasks"    . ?t)
                        ("hard tasks"    . ?T)
                        ("work"          . ?w)
                        ("side projects" . ?s)
                        ("health"        . ?h)
                        ("relationships" . ?r)
                        ("emacs"         . ?e)
                        ("A"             . ?a)
                        ("B"             . ?b)
                        ("C"             . ?c)
                        ("music"         . ?m)))

  (defvar custom-functions-list '(evil-org-open-above evil-org-open-below  org-return))

  (dolist (func custom-functions-list)
    (advice-add func :after
                (lambda (&rest args)
                  (+org-indent))))

  ;; Don't execute org-babel blocks on export
  (add-to-list 'org-babel-default-header-args
               '(:eval . "never-export"))

  ;; Org-mode local settings
  (add-hook 'org-mode-hook (lambda ()
                             (add-to-list 'org-modules 'habits)))

  ;; No unnecessary background highlighting
  (custom-set-faces
   `(org-block            ((t (:background ,bg-color))))
   `(org-block-begin-line ((t (:background ,bg-color))))
   `(org-block-end-line   ((t (:background ,bg-color))))
   `(org-level-1          ((t (:background ,bg-color))))
   `(org-quote            ((t (:background ,bg-color))))
   `(org-headline-done    ((t (:strike-through t :foreground "#7C6f64"))))
   `(org-done             ((t (:foreground "#7C6f64")))))

  (setq org-todo-keyword-faces '(("WAIT" . (:foreground "#7C6f64" :weight bold))
                                 ("OPT." . (:foreground "#fe8019" :weight bold)))

        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)"
                                      "OPT.(o)" "WIP.(p)" "|" "DONE")))

  (plist-put org-format-latex-options :scale 1.75)) ; Larger inline org latex
