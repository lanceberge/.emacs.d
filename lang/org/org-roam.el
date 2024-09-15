;;; -*- lexical-binding: t -*-
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
      :unnarrowed t)))
  :general
  (my-leader-def
    "oni" #'(org-roam-node-insert :which-key "insert link")
    "onn" #'(+org-roam-node-insert-immediate :which-key "insert now")
    "ont" #'(+org-roam-add-todo :which-key "add todo")
    "ond" #'(+org-roam-add-drill-tag :which-key "add drill tag")
    "onp" #'(+org-roam-add-project-tag :which-key "add project tag")
    "onq" #'(org-roam-tag-add :which-key "add tag"))
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
                             817))) ; U+0331 COMBINING MACRON BELOW
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
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  :general
  (my-leader-def
    "ong" #'(org-roam-ui-mode :which-key "org roam graph")))

(use-package consult-org-roam
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :general
  (my-leader-def
    "onb" #'(consult-org-roam-backlinks :which-key "view backlinks")
    "onl" #'(consult-org-roam-forward-links :which-key "view forward links")
    "on SPC b" #'(consult-org-roam-backlinks-recursive :which-key "view recursive backlinks")
    "onf" #'(consult-org-roam-file-find :which-key "find note"))
  :config
  (consult-org-roam-mode))

;;;###autoload
(defun +org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;;;###autoload
(defun +org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;;###autoload
(defun +org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (+org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

;;;###autoload
(defun +org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (append (+org-roam-list-notes-by-tag "Project") org-agenda-files)))

;;;###autoload
(defun +org-roam-project-finalize-hook ()
  (remove-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)

  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

;;;###autoload
(defun +org-roam-add-todo ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)
  (org-roam-capture-
   :node
   (org-roam-node-read nil (+org-roam-filter-by-tag "Project"))
   :templates
   '(("p" "project" plain "** TODO %?"
      :if-new
      (file+head+olp "projects/${slug}.org"
                     "#+title: ${title}\n#+filetags: Project ${slug}"
                     ("Tasks"))))))

;;;###autoload
(defun +org-roam-add-project-tag ()
  "Add a project tag and add this to org-drill files"
  (interactive)
  (org-roam-tag-add '("Project"))
  (add-to-list 'org-agenda-files (buffer-file-name)))

;;;###autoload
(defun +org-roam-add-drill-tag ()
  "Add a drill tag and add this to org-drill files"
  (interactive)
  (org-roam-tag-add '("Drill"))
  (add-to-list 'org-drill-scope (buffer-file-name)))
