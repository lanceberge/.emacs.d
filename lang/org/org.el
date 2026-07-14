;;; -*- lexical-binding: t -*-
(use-package org
  :defer 4.0
  :custom
  (org-directory "~/org")
  (org-list-allow-alphabetical t)
  (org-startup-folded t)
  (org-fontify-done-headline t)
  (org-M-RET-may-split-line nil)
  (org-return-follows-link t)
  (org-modules nil)
  (org-image-actual-width nil)
  (org-export-backends '(html md))
  (org-special-ctrl-a t)
  (org-todo-keywords '((sequence
                        "TODO"
                        "IN PROGRESS(p!)"
                        "BLOCKED(b@)"
                        "|"
                        "IN REVIEW(r)"
                        "DONE(d!)"
                        "CANCELED(c@)")))
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-confirm-elisp-link-function nil)
  :bind
  (:map org-mode-map
        ([remap consult-imenu] . #'consult-org-heading)
        ([remap +drag-stuff-down] . #'org-metadown)
        ([remap newline] . #'org-return)
        ([remap +drag-stuff-up] . #'org-metaup)
        ([remap +puni-kill-line-dwim] . #'org-kill-line)
        ("C-j" . #'org-insert-heading-respect-content)
        ("C-S-j" . #'org-insert-todo-heading-respect-content)
        ("M-<return>" . #'+modal-org-meta-return-insert)
        ("M-S-<return>" . #'+modal-org-insert-todo-heading-insert)
        ("M-l" . #'org-shiftmetaright)
        ("M-h" . #'org-shiftmetaleft)
        ("C-c TAB" . #'outline-toggle-children)
        ("C-c m" . #'outline-toggle-sublevels)
        ("C-c r" . #'outline-show-all))
  :config
  (+modal-create-insert-function org-meta-return)
  (+modal-create-insert-function org-insert-todo-heading))

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
  :bind
  (:map +leader-map
        ("ont" . #'+org-project-add-todo)
        ("ond" . #'+org-project-mark-done)
        ("onx" . #'+org-project-add-done)
        ("one" . #'+org-project-edit-todo)
        ("onr" . #'+org-project-reset-project-file-for-current-project))
  :config
  (add-to-list 'savehist-additional-variables
               '+org-project--project-files-for-dir))

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
