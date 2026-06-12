;;; -*- lexical-binding: t -*-
(use-package org
  :defer 2.0
  :hook
  (org-mode . (lambda () (setq-local tab-width 2)))
  :custom
  (org-directory "~/org")
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
        ([remap consult-imenu] . #'consult-org-heading)
        ([remap insert-newline-indent] . #'org-return)
        ("M-H" . org-shifleft)
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
                        ("Music" . ?m)
                        ("drill" . ?d)))

  (add-to-list 'org-babel-default-header-args
               '(:eval . "never-export"))
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (plist-put org-format-latex-options :scale 1.75))

(use-package org-extensions
  :ensure (:type file :main "~/.emacs.d/packages/org-extensions.el")
  :after org
  :custom
  (+org-directory "~/org/")
  :bind
  (:map org-mode-map
        ([remap drag-stuff-up] . #'+org-drag-stuff-up)
        ([remap drag-stuff-down] . #'+org-drag-stuff-down)
        ([remap +drag-stuff-left-dwim] . #'+org-metaleft-dwim)
        ([remap +drag-stuff-right-dwim] . #'+org-metaright-dwim)
        ([remap next-line] . #'+org-down)
        ([remap previous-line] . #'+org-up)
        ("M-l" . #'+org-metaright-dwim)
        ("M-h" . #'+org-metaleft-dwim))
  (:map +leader-map
        ("of" . #'+org-find-file)))

(use-package modal-org
  :ensure (:type file :main "~/.emacs.d/packages/modal-org.el")
  :after (modal org)
  :bind
  (:map org-mode-map
        ("M-<return>" . #'+modal-org-insert-todo)
        ("M-S-<return>" . #'+modal-org-insert-todo-above)
        ("C-<return>" . #'+modal-org-insert-heading)
        ("C-S-<return>" . #'+modal-org-insert-heading-above)
        ([remap +open-below] . #'+modal-org-insert-below)
        ([remap +open-above] . #'+modal-org-insert-above)))

(use-package +org-project
  :ensure (:type file :main "~/.emacs.d/packages/org-project.el")
  :bind
  (:map +leader-map
        ("ont" . #'+org-project-add-todo)
        ("ond" . #'+org-project-mark-done)
        ("onx" . #'+org-project-add-done)))
