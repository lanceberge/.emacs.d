;;; -*- lexical-binding: t -*-
(use-package org-agenda
  :defer 0.5
  :straight (:type built-in)
  :custom
  (org-agenda-span 14)              ; show 14 days
  (org-agenda-start-on-weekday nil) ; start on today
  (org-agenda-files '("~/org/todo.org"))
  :general
  ('org-agenda-mode-map
   [remap org-agenda-todo] #'org-agenda-filter)

  (my-leader-def
    "oa" (lambda ()
           (interactive)
           (org-agenda nil "t")))
  :config
  (require 'evil-org-agenda)
  (+org-roam-refresh-agenda-list)
  (evil-org-agenda-set-keys)
  (which-key-add-key-based-replacements
    "SPC oa" "agenda"))

(use-package org-roam
  :defer-incrementally (emacsql emacsqlite)
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
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
      :unnarrowed t)
     )
   )
  :general
  (my-leader-def
    "oni" #'(org-roam-node-insert            :which-key "insert link")
    "onl" #'(org-roam-buffer-toggle          :which-key "view links")
    "onn" #'(+org-roam-node-insert-immediate :which-key "insert now")
    "onf" #'(org-roam-node-find              :which-key "find note")
    "ont" #'(+org-roam-add-todo              :which-key "add todo")
    )
  :config
  (org-roam-db-autosync-mode))

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
      entry (file+headline "~/org/todo.org" "Emacs")
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
