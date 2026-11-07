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
(defun +format-org (&optional file)
  "Format headings and tables in the current Org buffer or FILE.
When FILE is non-nil, visit it, format it, and save it.  Interactively,
use a prefix argument to prompt for FILE."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Format Org file: " nil nil t))))
  (if file
      (with-current-buffer (find-file-noselect file)
        (+format-org)
        (let ((before-save-hook (remq #'+format-org before-save-hook)))
          (save-buffer)))
    (unless (derived-mode-p 'org-mode)
      (user-error "Not an Org buffer"))
    (org-save-outline-visibility t
      (let ((current-table (when (org-at-table-p)
                             (save-excursion (org-table-begin)))))
        (dolist (position
                 (reverse
                  (org-element-map (org-element-parse-buffer) 'table
                    (lambda (table)
                      (when (eq (org-element-property :type table) 'org)
                        (org-element-property :begin table))))))
          (when position
            (if (equal position current-table)
                (org-table-align)
              (save-excursion
                (goto-char position)
                (org-table-align)))))
        (let ((line (1+ (save-excursion
                          (beginning-of-line)
                          (let ((count 0))
                            (while (re-search-backward "^[ \t]*[^ \t\n]" nil t)
                              (setq count (1+ count)))
                            count))))
              (column (current-column))
              (blank (save-excursion
                       (beginning-of-line)
                       (looking-at-p "[ \t]*$"))))
          (+org-format-heading-spacing)
          (+org-restore-format-point line column blank))))))

;;;###autoload
(defun +org-restore-format-point (line column blank)
  "Restore point to nonblank LINE and COLUMN after formatting.
When BLANK is non-nil, use the blank line preceding LINE if one exists."
  (goto-char (point-min))
  (let ((count 0))
    (while (and (< count line)
                (re-search-forward "^[ \t]*[^ \t\n]" nil t))
      (setq count (1+ count)))
    (if (< count line)
        (goto-char (point-max))
      (beginning-of-line)
      (when (and blank
                 (save-excursion
                   (and (not (bobp))
                        (progn
                          (forward-line -1)
                          (looking-at-p "[ \t]*$")))))
        (forward-line -1))
      (move-to-column column))))

;;;###autoload
(defun +org-format-heading-spacing ()
  "Set spacing around heading lines in the current buffer."
  (goto-char (point-max))
  (beginning-of-line)
  (let (next-line next-heading done)
    (while (not done)
      (unless (looking-at-p "[ \t]*$")
        (let ((current-line (point))
              (current-heading (org-at-heading-p)))
          (when (and next-line (or current-heading next-heading))
            (let* ((gap-start (line-end-position))
                   (spacing (if (and current-heading next-heading)
                                "\n" "\n\n")))
              (unless (equal (buffer-substring-no-properties gap-start next-line)
                             spacing)
                (delete-region gap-start next-line)
                (goto-char gap-start)
                (insert spacing)))
            (goto-char current-line))
          (setq next-line current-line
                next-heading current-heading)))
      (if (bobp)
          (setq done t)
        (forward-line -1)))))

;;;###autoload
(defun +org-enable-format-on-save ()
  "Format this Org buffer before saving."
  (add-hook 'before-save-hook #'+format-org nil t))

;;;###autoload
(defun +org-up (_arg)
  "Fix for org going to the end of the ... w/ outline mode"
  (interactive "p")
  (call-interactively #'previous-line)
  (when (eolp)
    (org-end-of-line)))

;;;###autoload
(defun +org-down (_arg)
  (interactive "p")
  (call-interactively #'next-line)
  (when (eolp)
    (org-end-of-line)))

;;;###autoload
(defun +org-find-file ()
  (interactive)
  (+project-tab-switch-to-project +org-directory t)
  (project-find-file))

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
