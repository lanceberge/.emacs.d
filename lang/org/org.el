;;; -*- lexical-binding: t -*-
(use-package org
  :defer 2.0
  :init
  (+modal-bind '+leader-map 'org-mode-hook
               '(("i TAB" . outline-toggle-children)
                 ("in" . org-next-visible-heading)
                 ("ip" . org-previous-visible-heading)
                 ("ir" . outline-show-all)
                 ("im" . outline-hide-sublevels)
                 ("il" . org-toggle-link-display)))
  (+modal-bind '+normal-mode-map 'org-mode-hook
               '(([remap +puni-kill-line-dwim] . org-kill-line)))
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
  (org-special-ctrl-a t)
  (org-latex-listings 'minted) ; syntax-highlighted code blocks
  ;; (org-latex-packages-alist '(("margin=0.5in" "geometry" nil) (nil "minted" "color")))
  (org-latex-pdf-process ; required to use minted
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-minted-options '(("linenos" "true")
                              ("frame" "lines")
                              ("style" "emacs")))
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-confirm-elisp-link-function nil)
  :bind
  (:map org-mode-map
        ([remap consult-imenu] . #'consult-org-heading)
        ([remap +drag-stuff-down] . #'org-metadown)
        ([remap newline] . #'org-return)
        ([remap +drag-stuff-up] . #'org-metaup)
        ("C-j" . #'org-insert-heading-respect-content)
        ("C-S-j" . #'org-insert-todo-heading-respect-content)
        ("M-<return>" . #'+modal-org-meta-return-insert)
        ("M-S-<return>" . #'+modal-org-insert-todo-heading-insert)
        ("M-l" . #'org-shiftmetaright)
        ("M-h" . #'org-shiftmetaleft))
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

  (+modal-create-insert-function org-meta-return)
  (+modal-create-insert-function org-insert-todo-heading)

  (add-to-list 'org-babel-default-header-args
               '(:eval . "never-export"))
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (plist-put org-format-latex-options :scale 1.75))

(use-package org-extras
  :ensure (:type file :main "~/.emacs.d/lisp/org-extras.el" :files ("org-extras.el"))
  :after org
  :custom
  (+org-directory "~/org/")
  :bind
  (:map org-mode-map
        ([remap next-line] . #'+org-down)
        ([remap previous-line] . #'+org-up))
  (:map +leader-map
        ("of" . #'+org-find-file)))

(use-package org-project
  :ensure (:type file :main "~/.emacs.d/lisp/org-project/org-project.el" :files ("org-project.el"))
  :hook
  (savehist-mode . +org-project-savehist-mode)
  :config
  (require 'org-project-consult)
  (when (bound-and-true-p savehist-mode)
    (+org-project-savehist-mode 1))
  :bind
  (:map +leader-map
        ("ont" . #'+org-project-add-todo)
        ("ond" . #'+org-project-mark-done)
        ("onx" . #'+org-project-add-done)
        ("onr" . #'+org-project-reset-project-file-for-current-project)))

(use-package org-project-consult
  :ensure (:type file :main "~/.emacs.d/lisp/org-project/org-project-consult.el" :files ("org-project-consult.el"))
  :custom
  (+org-project-consult-preview-files nil)
  :config
  (+org-project-consult-mode 1)
  :bind
  (:map +leader-map
        ("onj" . #'+org-project-consult-file-for-current-project)
        ("onf" . #'+org-project-consult-file)
        ("ona" . #'+org-project-consult-agenda)))
