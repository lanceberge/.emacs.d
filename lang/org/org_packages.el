;;; -*- lexical-binding: t -*-
(use-package org-agenda
  :defer 0.5
  :after org-roam
  :straight (:type built-in)
  :custom
  (org-agenda-span 14)              ; show 14 days
  (org-agenda-start-on-weekday nil) ; start on today
  (org-agenda-files '("~/org/todo.org"))
  (org-agenda-tags-column 0)
  (org-agenda-custom-commands
   '(
     ("w" "Work"
      ((tags-todo "Work"))
      )

     ("p" "Projects"
      ((tags-todo "Project"))
      )
     ("e" "Emacs"
      ((tags-todo "Emacs")
       )
      )
     ))
  :general
  ('org-agenda-mode-map
   [remap org-agenda-todo] #'org-agenda-filter)

  (my-leader-def
    "oa" #'(org-agenda :which-key "org agenda"))
  :config
  (require 'evil-org-agenda)
  (+org-roam-refresh-agenda-list)
  (evil-org-agenda-set-keys)
  (which-key-add-key-based-replacements
    "SPC oa" "agenda"))

(use-package org-roam
  :commands (org-roam-node-list)
  :defer-incrementally (emacsql emacsqlite)
  :after org
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/org-roam")
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}")))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "Programming" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Programming\n")
      :unnarrowed t)
     ("p" "Projects" plain "%?"
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Project ${title}\n")
      :unnarrowed t)
     )
   )
  :general
  (my-leader-def
    "oni" #'(org-roam-node-insert            :which-key "insert link")
    "onn" #'(+org-roam-node-insert-immediate :which-key "insert now")
    "ont" #'(+org-roam-add-todo              :which-key "add todo")
    "ond" #'(+org-roam-add-drill-tag         :which-key "add todo")
    "onp" #'(+org-roam-add-project-tag       :which-key "add todo")
    )
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  :general
  (my-leader-def
    "ong" #'(org-roam-ui-mode :which-key "org roam graph")
    )
  )

(use-package consult-org-roam
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :general
  (my-leader-def
    "onb"      #'(consult-org-roam-backlinks           :which-key "view backlinks")
    "onl"      #'(consult-org-roam-forward-links       :which-key "view forward links")
    "on SPC b" #'(consult-org-roam-backlinks-recursive :which-key "view recursive backlinks")
    "onf"      #'(consult-org-roam-file-find           :which-key "find note")
    )
  )

(use-package org-capture
  :straight (:type built-in)
  :custom
  (org-capture-templates
   '(("a" "A"
      entry (file+headline "~/org/todo.org" "A")
      "** TODO %?\n %i\n" :prepend t)

     ("b" "B"
      entry (file+headline "~/org/todo.org" "B")
      "** TODO %?\n %i\n" :prepend t)

     ("c" "C"
      entry (file+headline "~/org/todo.org" "C")
      "** TODO %?\n %i\n" :prepend t)

     ("w" "Work"
      entry (file+headline "~/org/todo.org" "Work")
      "** TODO %?\n %i\n" :prepend t)

     ("m" "Music"
      entry (file+headline "~/org/todo.org" "Music")
      "** TODO %?\n %i\n" :prepend t)

     ("e" "Emacs"
      entry (file+headline "~/org-roam/20240804163601-emacs.org" "Tasks")
      "** TODO %?\n %i\n" :prepend t)))
  :general
  (my-leader-def
    "oc" #'(org-capture :which-key "org capture"))
  :config
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-src
  :straight (:type built-in)
  :defer t
  :custom
  (org-edit-src-content-indentation 0) ; leading spaces before the #+begin line
  (org-src-preserve-indentation t)     ; don't preserve leading whitespace on export
  (org-adapt-indentation nil)          ; don't indent under headlines

  (org-src-window-setup 'current-window)
  :config
  (setq org-src-tab-acts-natively t))

(use-package evil-org ; functions to work with evil-mode in org-mode
  :general
  ;; bind evil-org functions manually rather than using evil-org-mode, which has some
  ;; conflicting bindings for my preferences
  ('normal org-mode-map
           "o" #'evil-org-open-below
           "O" #'evil-org-open-above)

  ('(normal insert) org-mode-map
   "C-;"   #'evil-org-org-insert-heading-respect-content-below
   "M-;"   #'evil-org-org-insert-todo-heading-respect-content-below))

(use-package org-drill
  :after org-roam
  :general
  (my-leader-def
    "od" #'(org-drill :which-key "org-drill")
    )
  :init
  (setq org-drill-scope
        (if (setq org-drill-scope-list (+org-roam-list-notes-by-tag "Drill"))
            org-drill-scope-list
          'file))
  )


(use-package org-modern
  :custom
  (org-modern-star 'replace)
  :custom-face
  (org-modern-face ((t (:background "#d3869b"))))
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

  ('normal org-journal-mode-map
           "za" #'(org-cycle    :which-key "open fold")))

(use-package ob ; org babel
  :straight (:type built-in)
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   "b"  #'(org-babel-tangle :which-key "tangle file")))

(use-package ox ; org exports
  :straight (:type built-in)
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   "e" #'(org-export-dispatch :which-key "export")))

(use-package ol ; org links
  :straight (:type built-in)
  :general
  ('(normal insert) org-mode-map
   :prefix "C-c"
   ",l" #'(org-insert-link :which-key "insert link"))
  ('override
   :prefix "C-c"
   "l"  #'(org-store-link  :which-key "store link")))

;; autoload org babel functions for specific languages
(use-package ob-haskell
  :straight (:type built-in)
  :commands org-babel-execute:haskell)

(use-package ob-shell
  :straight (:type built-in)
  :commands org-babel-execute:sh)

(use-package ob-C
  :straight (:type built-in)
  :commands org-babel-execute:C)

(use-package ob-R
  :straight (:type built-in)
  :commands org-babel-execute:R)

(use-package ob-python
  :straight (:type built-in)
  :commands org-babel-execute:python)

(use-package ob-matlab
  :straight (:type built-in)
  :commands org-babel-execute:matlab)

(use-package toc-org ; auto-generate tables of contents w/in org and markdown with a :TOC: tag
  :hook ((org-mode markdown-mode) . toc-org-mode))
