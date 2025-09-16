;;; -*- lexical-binding: t -*-
(use-package org
  :ensure (:wait t)
  :defer 2.0
  :defer-incrementally
  (calendar find-func format-spec org-macs
            org-faces org-entities org-list org-src
            ob org org-agenda org-capture flyspell
            org-element)
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "notes.org/" org-directory ))
  (org-list-allow-alphabetical t)
  (org-startup-folded t)
  (org-fontify-done-headline t)
  (org-M-RET-may-split-line nil)
  (org-return-follows-link t)
  (org-modules nil)
  (org-image-actual-width nil)

  (org-export-backends '(html latex md))
  (org-latex-listings 'minted) ; syntax-highlighted code blocks
  ;; (org-latex-packages-alist '(("margin=0.5in" "geometry" nil) (nil "minted" "color")))
  (org-latex-pdf-process ; required to use minted
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-minted-options '(("linenos" "true")
                              ("frame" "lines")
                              ("style" "emacs")))
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  :bind
  (:map org-mode-map
        ("M-<return>" . +org/insert-todo)
        ("M-S-<return>" . +org/insert-todo-above)
        ("C-<return>" . +org/insert-heading)
        ("C-S-<return>" . +org/insert-heading-above)
        ("M-h" . org-metaleft)
        ([remap meow-open-below] . +org-insert-below)
        ([remap meow-open-above] . +org-insert-above)
        ([remap drag-stuff-up] . #'+org-drag-stuff-up)
        ([remap drag-stuff-down] . #'+org-drag-stuff-down)
        ([remap org-cycle] . #'+org-cycle)
        ("M-[" . org-metaleft)
        ("M-]" . org-metaright)
        ("M-H" . org-shiftleft)
        ("M-J" . org-shiftdown)
        ("M-K" . org-shiftup)
        ("M-L" . org-shiftright)
        ("C-M-h" . org-shiftmetaleft)
        ("C-M-j" . org-shiftmetadown)
        ("C-M-k" . org-shiftmetaup)
        ("C-M-l" . org-shiftmetaright)
        ("C-S-h" . org-shiftcontrolleft)
        ("C-S-j" . org-shiftcontroldown)
        ("C-S-k" . org-shiftcontrolup)
        ("C-S-l" . org-shiftcontrolright))

  :config
  (defun org-mode-company-backends ()
    (setq-local company-backends
                '((company-files company-capf))))

  (add-hook 'org-mode-hook 'org-mode-company-backends)

  (setq org-tag-alist '(("personal" . ?p)
                        ("easy tasks" . ?t)
                        ("hard tasks" . ?T)
                        ("Work" . ?w)
                        ("side projects" . ?s)
                        ("health" . ?h)
                        ("Emacs" . ?e)
                        ("A" . ?a)
                        ("B" . ?b)
                        ("C" . ?c)
                        ("Music" . ?m)))

  (add-to-list 'org-babel-default-header-args
               '(:eval . "never-export"))


  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; (setq org-todo-keyword-faces '(("WAIT" . (:foreground "#7C6f64" :weight bold))
  ;;                                ("OPT." . (:foreground "#fe8019" :weight bold)))

  ;;       org-todo-keywords '((sequence "TODO(t)" "WAIT(w)"
  ;;                                     "OPT.(o)" "WIP.(p)" "|" "DONE")))

  (plist-put org-format-latex-options :scale 1.75))
