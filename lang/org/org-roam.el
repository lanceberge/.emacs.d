;;; -*- lexical-binding: t -*-
(use-package org-roam
  :commands (org-roam-node-list org-roam-node-slug)
  :defer-incrementally (emacsql emacsqlite)
  :after org
  :defer 2.1
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
  :bind
  (:map +leader-map
        ("oni" . #'org-roam-node-insert)
        ("onn" . #'+org-roam-node-insert-immediate)
        ("ont" . #'+org-roam-add-todo)
        ("ond" . #'+org-roam-find-drill-files)
        ("onp" . #'+org-roam-add-project-tag)
        ("onq" . #'org-roam-tag-add))
  :config
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(768 769 770 771 772 774 775 776 777 778 779 780 795 803 804 805 807 813 814 816 817)))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                        ("__*" . "_") ;; remove sequential underscores
                        ("^_" . "") ;; remove starting underscore
                        ("_$" . ""))) ;; remove ending underscore
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
  :bind
  (:map +leader-map
        ("ong" . #'org-roam-ui-mode)))

(use-package consult-org-roam
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :commands
  (consult-org-roam-file-find)
  :bind
  (:map +leader-map
        ("onb" . #'consult-org-roam-backlinks)
        ("onl" . #'consult-org-roam-forward-links)
        ("of" . #'+org-find-file)
        ("on SPC b" . #'consult-org-roam-backlinks-recursive))
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
    (and (equal (org-roam-node-level node) 0)
         (member tag-name (org-roam-node-tags node)))))

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
