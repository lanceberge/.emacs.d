;;; -*- lexical-binding: t -*-
(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-span 14)              ; show 14 days
  (org-agenda-start-on-weekday nil) ; start on today
  (org-agenda-tags-column 0)
  (org-agenda-custom-commands
   '(("w" "Work"
      ((tags-todo "Work"))
      ((org-agenda-sorting-strategy '(priority-down))))

     ("p" "Projects"
      ((tags-todo "Project"))
      ((org-agenda-sorting-strategy '(priority-down))))

     ("e" "Emacs"
      ((tags-todo "Emacs"))
      ((org-agenda-sorting-strategy '(priority-down))))

     ("l" "Life"
      ((tags-todo "-Work-Emacs-Programming"))
      ((org-agenda-sorting-strategy '(priority-down))))))
  :general
  (my-leader-def
    "oa" #'(org-agenda :which-key "org agenda"))
  ('org-agenda-mode
   "g" #'ace-link)


  ('motion 'org-agenda-mode-map
           ";"  #'org-agenda-switch-to
           "go" #'ace-link
           [remap org-agenda-todo] #'org-agenda-filter)

  :config
  (require 'evil-org-agenda)
  (+org-roam-refresh-agenda-list)
  (evil-org-agenda-set-keys)
  (which-key-add-key-based-replacements
    "SPC oa" "agenda"))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   '(("a" "A"
      entry (file+headline "~/org-roam/projects/todo.org" "A")
      "** TODO %?\n %i\n" :prepend t)

     ("b" "B"
      entry (file+headline "~/org-roam/projects/todo.org" "B")
      "** TODO %?\n %i\n" :prepend t)

     ("c" "C"
      entry (file+headline "~/org-roam/projects/todo.org" "C")
      "** TODO %?\n %i\n" :prepend t)

     ("w" "Work"
      entry (file+headline "~/org-roam/projects/work.org" "Tasks")
      "** TODO %?\n %i\n" :prepend t)

     ("e" "Emacs"
      entry (file+headline "~/org-roam/projects/emacs.org" "Tasks")
      "** TODO %?\n %i\n" :prepend t)))
  :general
  (my-leader-def
    "oc" #'(org-capture :which-key "org capture"))
  :config
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-src
  :ensure nil
  :defer t
  :custom
  (org-edit-src-content-indentation 0) ; leading spaces before the #+begin line
  (org-src-preserve-indentation t)     ; don't preserve leading whitespace on export
  (org-adapt-indentation t)

  (org-src-window-setup 'current-window)
  :config
  (setq org-src-tab-acts-natively t))

(use-package evil-org ; functions to work with evil-mode in org-mode
  :general
  ;; bind evil-org functions manually rather than using evil-org-mode, which has some
  ;; conflicting bindings for my preferences
  ('meow-normal-state-keymap org-mode-map
                             "o" #'evil-org-open-below
                             "O" #'evil-org-open-above)

  ('(normal insert) org-mode-map
   "C-;" #'evil-org-org-insert-heading-respect-content-below
   "M-;" #'evil-org-org-insert-todo-heading-respect-content-below))

(use-package org-drill
  :after org-roam
  :general
  (my-leader-def
    "od" #'(org-drill :which-key "org-drill"))
  (setq org-drill-scope
        (if (setq org-drill-scope-list (+org-roam-list-notes-by-tag "Drill"))
            org-drill-scope-list
          'file)))

(use-package org-modern
  :custom
  (org-modern-star 'replace)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-journal
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-file-format "%m%d%Y")
  :general
  (my-leader-def
    "ojn" #'(org-journal-new-entry :which-key "new"))

  ('meow-normal-state-keymap org-journal-mode-map
                             "za" #'(org-cycle :which-key "open fold")))

(use-package ob ; org babel
  :ensure nil
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   "b"  #'(org-babel-tangle :which-key "tangle file")))

(use-package ox ; org exports
  :ensure nil
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   "e" #'(org-export-dispatch :which-key "export")))

(use-package ol ; org links
  :ensure nil
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   ",l" #'(org-insert-link :which-key "insert link"))
  ('override
   :prefix "C-c"
   "l"  #'(org-store-link :which-key "store link")))

;; autoload org babel functions for specific languages
(use-package ob-haskell
  :ensure nil
  :commands org-babel-execute:haskell)

(use-package ob-shell
  :ensure nil
  :commands org-babel-execute:sh)

(use-package ob-C
  :ensure nil
  :commands org-babel-execute:C)

(use-package ob-R
  :ensure nil
  :commands org-babel-execute:R)

(use-package ob-python
  :ensure nil
  :commands org-babel-execute:python)

(use-package ob-matlab
  :ensure nil
  :commands org-babel-execute:matlab)

(use-package ob-perl
  :ensure nil
  :commands org-babel-execute:perl)

(use-package toc-org ; auto-generate tables of contents w/in org and markdown with a :TOC: tag
  :hook ((org-mode markdown-mode) . toc-org-mode))
