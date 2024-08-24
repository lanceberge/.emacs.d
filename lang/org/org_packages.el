;;; -*- lexical-binding: t -*-
(use-package org-agenda
  :defer 1.5
  :straight (:type built-in)
  :custom
  (org-agenda-span 14)              ; show 14 days
  (org-agenda-start-on-weekday nil) ; start on today
  (org-agenda-tags-column 0)
  (org-agenda-custom-commands
   '(
     ("w" "Work"
      ((tags-todo "Work"))
      ((org-agenda-sorting-strategy '(priority-down)))
      )

     ("p" "Projects"
      ((tags-todo "Project"))
      ((org-agenda-sorting-strategy '(priority-down)))
      )
     ("e" "Emacs"
      ((tags-todo "Emacs"))
      ((org-agenda-sorting-strategy '(priority-down))
       )
      )
     ("l" "Life"
      ((tags-todo "-Work-Emacs-Programming"))
      ((org-agenda-sorting-strategy '(priority-down))
       )
      )
     ))
  :general
  ('org-agenda-mode
   "g" #'ace-link)
  (my-leader-def
    "oa" #'(org-agenda :which-key "org agenda"))

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
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "Programming" plain "%?"
      :if-new (file+head "public/${slug}.org"
                         "#+title: ${title}\n#+filetags: Programming\n")
      :unnarrowed t)
     ("p" "Projects" plain "%?"
      :if-new
      (file+head "projects/${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Project ${title}\n")
      :unnarrowed t)
     ("b" "Braindump (public)" plain "%?"
      :if-new
      (file+head "public/${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n")
      :unnarrowed t)
     )
   )
  :general
  (my-leader-def
    "oni" #'(org-roam-node-insert            :which-key "insert link")
    "onn" #'(+org-roam-node-insert-immediate :which-key "insert now")
    "ont" #'(+org-roam-add-todo              :which-key "add todo")
    "ond" #'(+org-roam-add-drill-tag         :which-key "add drill tag")
    "onp" #'(+org-roam-add-project-tag       :which-key "add project tag")
    "onq" #'(org-roam-tag-add                :which-key "add tag")
    )
  :config
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                        ("__*" . "_")                   ;; remove sequential underscores
                        ("^_" . "")                     ;; remove starting underscore
                        ("_$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          slug))))

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
  :config
  (consult-org-roam-mode)
  )

(use-package org-capture
  :straight (:type built-in)
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
  :straight (:type built-in)
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

(use-package ob-perl
  :straight (:type built-in)
  :commands org-babel-execute:perl)

(use-package toc-org ; auto-generate tables of contents w/in org and markdown with a :TOC: tag
  :hook ((org-mode markdown-mode) . toc-org-mode))
