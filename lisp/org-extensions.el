;;; org-extensions.el --- Org editing helpers -*- lexical-binding: t -*-

(require 'org)
(require 'project)

(defgroup +org nil
  "Org editing extensions."
  :group 'org)

(defcustom +org-directory "~/org/"
  "Directory used by `+org-find-file'."
  :type 'directory
  :group '+org)

;;;###autoload
(defun +org-up (&optional arg)
  "Fix for org going to the end of the ... w/ outline mode"
  (interactive "p")
  (previous-line arg)
  (when (eolp)
    (org-end-of-line))
  (deactivate-mark))

;;;###autoload
(defun +org-down (&optional arg)
  (interactive "p")
  (next-line arg)
  (when (eolp)
    (org-end-of-line))
  (deactivate-mark))

;;;###autoload
(defun +org-find-file ()
  (interactive)
  (let* ((org-dir (expand-file-name +org-directory))
         (pr (cons 'transient org-dir)))
    (project-find-file-in nil (list org-dir) pr)))


;;;###autoload
(defun +org-metaleft-dwim (arg)
  (interactive "P")
  (if (not (region-active-p))
      (org-metaleft arg)
    (+drag-stuff-left-dwim (if (full-line-region-p) 2 arg))))

;;;###autoload
(defun +org-metaright-dwim (arg)
  (interactive "P")
  (if (not (region-active-p))
      (org-metaright arg)
    (+drag-stuff-right-dwim (if (full-line-region-p) 2 arg))))

;;;###autoload
(defun +org--has-filetag-p (tag)
  (and (member tag (+org--get-filetags)) t))

;;;###autoload
(defun +org--get-filetags ()
  "Return a list of filetags from the current buffer."
  (and (eq major-mode 'org-mode)
       (let ((filetags (car (org-collect-keywords '("FILETAGS")))))
         (when filetags
           (split-string (cadr filetags) ":" t "\\s-*")))))

(provide 'org-extensions)
;;; org-extensions.el ends here
