;;; -*- lexical-binding: t -*-
(use-package org-drill
  :bind
  (:map +leader-map
        ;; ("SPC od" . +org-drill-tag)
        ("od" . #'+org-drill-file))
  (:map +leader3-map
        ("od" . #'org-drill))
  :config
  ;; (setq org-drill-scope
  ;;       (if (setq org-drill-scope-list (+org-roam-list-notes-by-tag "Drill"))
  ;;           org-drill-scope-list
  ;;         'file))
  )

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
    ;; TODO replace w/ non org-roam
    (+org-roam-find-drill-files))
  (org-drill 'file nil nil nil))
