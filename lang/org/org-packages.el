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
  :bind
  (:map org-agenda-mode-map
        ("g" . ace-link))
  :general
  (my-leader-def
    "oa" #'(org-agenda :which-key "org agenda"))
  :config
  (+org-roam-refresh-agenda-list)
  (which-key-add-key-based-replacements
    "SPC oa" "agenda"))

(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . meow-insert-mode)
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
  :bind
  (:map org-capture-mode-map
        ([remap save-buffer] . org-capture-finalize))
  :general
  (my-leader-def
    "oc" #'(org-capture :which-key "org capture")))

(use-package calendar
  :ensure nil
  :defer t
  :bind
  (:map calendar-mode-map
        (";" . exit-minibuffer)
        ("M-l" . calendar-forward-day)
        ("M-h" . calendar-backward-day)
        ("M-j" . calendar-forward-week)
        ("M-k" . calendar-backward-week)
        ("M-H" . calendar-backward-month)
        ("M-L" . calendar-forward-month)
        ("M-K" . calendar-backward-year)
        ("M-J" . calendar-forward-year)))

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
    "ojn" #'(org-journal-new-entry :which-key "new")))

(use-package ob ; org babel
  :ensure nil
  :defer t
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ox ; org exports
  :ensure nil
  :defer t)

(use-package ol ; org links
  :ensure nil
  :defer t)

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

(use-package toc-org
  :hook ((org-mode markdown-mode) . toc-org-mode))
