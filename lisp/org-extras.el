;;; org-extras.el --- Org editing helpers -*- lexical-binding: t -*-

(require 'org)
(require 'project)
(require 'org-ql)

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
    (org-end-of-line)))

;;;###autoload
(defun +org-down (&optional arg)
  (interactive "p")
  (next-line arg)
  (when (eolp)
    (org-end-of-line)))

;;;###autoload
(defun +org-find-file ()
  (interactive)
  (let* ((org-dir (expand-file-name +org-directory))
         (pr (cons 'transient org-dir)))
    (project-find-file-in nil (list org-dir) pr)))

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

;;;###autoload
(defun +org-agenda-save-all-org-buffers ()
  "Save Org buffers and refresh the current or visible agenda."
  (interactive)
  (org-save-all-org-buffers)
  (if (derived-mode-p 'org-agenda-mode)
      (org-agenda-redo)
    (org-agenda-maybe-redo)))

;;;###autoload
(defun +org-blocked-by-open-todos-in-file (change-plist)
  "Block done transitions while inherited BLOCKED_BY_OPEN_TODOS_IN has open TODOs."
  (if-let* ((file (org-entry-get nil "BLOCKED_BY_OPEN_TODOS_IN" t))
            (state (plist-get change-plist :to))
            (_done (or (eq state 'done)
                       (member state org-done-keywords)))
            (_open-todos (org-ql-select file '(todo) :action '(point))))
      (progn
        (setq org-block-entry-blocking
              (format "open TODOs in %s" (abbreviate-file-name file)))
        nil)
    t))

(provide 'org-extras)
;;; org-extras.el ends here
