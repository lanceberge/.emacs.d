;;; -*- lexical-binding: t -*-
(setq tab-bar-show nil)

;;;###autoload
(defun +tab-bar/open-and-rename ()
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'tab-bar-rename-tab))

;;;###autoload
(defun +open-tab-if-exists (tab-name)
  (let ((tabs (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
    (if (member tab-name tabs)
        (tab-bar-switch-to-tab tab-name)
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab tab-name)))))
