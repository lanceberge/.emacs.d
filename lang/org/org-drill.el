;;; -*- lexical-binding: t -*-
(use-package org-drill
  :bind
  (:map +leader-map
        ("SPC od" . +org-drill-tag)
        ("od" . #'+org-drill-file))
  (:map +leader3-map
        ("od" . #'org-drill))
  :config
  (setq org-drill-scope
        (if (setq org-drill-scope-list (+org-roam-list-notes-by-tag "Drill"))
            org-drill-scope-list
          'file)))

;;;###autoload
(defun +org-roam-find-drill-files ()
  "Find/create an org drill file."
  (interactive)
  (let ((node (org-roam-node-read nil (+org-roam-filter-by-tag "Drill"))))
    (if (org-roam-node-file node)
        (org-roam-node-visit node)
      (org-roam-capture-
       :node node
       :templates
       '(("d" "drill" plain ""
          :target (file+head+olp "drill/${slug}.org"
                                 "#+title: ${title}\n#+filetags: Drill"
                                 (""))
          :unnarrowed t
          :immediate-finish t
          :jump-to-captured t))))))

;;;###autoload
(defun +org-roam-add-drill-tag ()
  "Add a drill tag and add this to org-drill files."
  (interactive)
  (org-roam-tag-add '("Drill"))
  (add-to-list 'org-drill-scope (buffer-file-name)))

;;;###autoload
(defun +org-drill-tag ()
  "Add the org-drill tag to this heading/file."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (user-error "must be in org mode"))
  (org-set-tags "drill")
  (+org-roam-add-drill-tag))

;;;###autoload
(defun +org-drill-file ()
  "Review the current file or prompt org drill files if this isn't one."
  (interactive)
  (unless (+org--has-filetag-p "Drill")
    (+org-roam-find-drill-files))
  (org-drill 'file nil nil nil))
