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
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "notes.org/" org-directory ))

  ;; General settings
  (org-list-allow-alphabetical t)
  (org-startup-folded t)
  (org-fontify-done-headline t)
  (org-M-RET-may-split-line nil)
  (org-return-follows-link t)
  (org-modules nil)

  ;; Latex exports
  (org-export-backends '(html latex md))
  (org-latex-listings 'minted) ; syntax-highlighted code blocks
  ;; (org-latex-packages-alist '(("margin=0.5in" "geometry" nil) (nil "minted" "color")))
  (org-latex-pdf-process ; required to use minted
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-minted-options '(("linenos" "true") ; line numbers in expored src blocks
                              ("frame" "lines")
                              ("style" "emacs")))

  ;; Styling
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
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
   "C-S-<return>" #'+org/insert-heading-above

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
           "zm" #'(outline-hide-sublevels :which-key "hide all")
           "RET" #'org-return)

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
  :custom-face
  (cursor                    ((t (:foreground "#928374"))))
  (org-block                 ((t (:foreground "#ebdbb2":background "#1c2021" :extend t))))
  (org-block-begin-line      ((t (:inherit org-block :background "#1d2021" :foreground "#665c54" :extend t))))
  (org-block-end-line        ((t (:inherit org-block-begin-line :background "#1d2021"))))
  (org-code                  ((t (:inherit org-verbatim :background "#282828" :foreground "#fe8019"))))
  (org-document-info         ((t (:foreground "#d5c4a1" :weight bold))))
  (org-document-info-keyword ((t (:inherit shadow))))
  (org-document-title        ((t (:foreground "#fbf1c7" :weight bold :height 1.2))))
  (org-meta-line             ((t (:inherit shadow))))
  (org-target                ((t (:height 0.7 :inherit shadow))))
  (org-link                  ((t (:foreground "#b8bb26" :background "#282828" :overline nil))))  ;;
  (org-indent                ((t (:inherit org-hide))))
  (org-indent                ((t (:inherit (org-hide fixed-pitch)))))
  (org-footnote              ((t (:foreground "#8ec07c" :background "#32302f" :overline nil))))
  (org-ref-cite-face         ((t (:foreground "#fabd2f" :background "#32302f" :overline nil))))  ;;
  (org-ref-ref-face          ((t (:foreground "#83a598" :background "#32302f" :overline nil))))
  (org-ref-label-face        ((t (:inherit shadow :box t))))
  (org-drawer                ((t (:inherit shadow))))
  (org-property-value        ((t (:inherit org-document-info))) t)
  (org-tag                   ((t (:inherit shadow))))
  (org-date                  ((t (:foreground "#83a598" :underline t))))
  (org-verbatim              ((t (:inherit org-block :background "#3c3836" :foreground "#d5c4a1"))))
  (org-quote                 ((t (:inherit org-block :slant italic))))
  (org-level-1               ((t (:foreground "#83a598" :background "#282828" :height 1.05 :overline nil :extend t))))
  (org-level-2               ((t (:foreground "#fe8019" :background "#282828" :height 1.05 :overline nil :extend t))))
  (org-level-3               ((t (:foreground "#d3869b" :background "#282828" :height 1.05 :overline nil :extend t))))
  (org-level-4               ((t (:foreground "#b8bb26" :background "#282828" :height 1.05 :overline nil :extend t))))
  :config
  (defun org-mode-company-backends ()
    (setq-local company-backends
                '((company-files company-capf))))

  (add-hook 'org-mode-hook 'org-mode-company-backends)

  (setq org-tag-alist '(("personal"      . ?p)
                        ("easy tasks"    . ?t)
                        ("hard tasks"    . ?T)
                        ("Work"          . ?w)
                        ("side projects" . ?s)
                        ("health"        . ?h)
                        ("relationships" . ?r)
                        ("Emacs"         . ?e)
                        ("A"             . ?a)
                        ("B"             . ?b)
                        ("C"             . ?c)
                        ("Music"         . ?m)))

  (setq kill-emacs-query-functions nil)
  (add-hook 'kill-emacs-hook
            (lambda () (interactive) (setq kill-emacs-query-functions nil)))

  (defvar custom-functions-list '(evil-org-open-above evil-org-open-below  org-return))

  (dolist (func custom-functions-list)
    (advice-add func :after
                (lambda (&rest args)
                  (+org-indent))))

  ;; Don't execute org-babel blocks on export
  (add-to-list 'org-babel-default-header-args
               '(:eval . "never-export"))


                                        ;  open links in the current window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-todo-keyword-faces '(("WAIT" . (:foreground "#7C6f64" :weight bold))
                                 ("OPT." . (:foreground "#fe8019" :weight bold)))

        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)"
                                      "OPT.(o)" "WIP.(p)" "|" "DONE")))

  (plist-put org-format-latex-options :scale 1.75)) ; Larger inline org latex
