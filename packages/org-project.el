;;; org-project.el --- Org capture helpers for tracking projects -*- lexical-binding: t -*-

(require 'org)
(require 'org-capture)

(defvar org-capture-templates)

(defvar +org-project-directory "~/org/projects/"
  "Directory containing project org files.")

;;;###autoload
(defun +org-project--prompt-file ()
  (completing-read "Select project file: "
                   (directory-files +org-project-directory t "\\.org$")
                   nil t))

;;;###autoload
(defun +org-project--todo-candidates ()
  "Return TODO heading candidates in the current Org buffer."
  (let (candidates)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-heading-regexp nil t)
       (when (equal (org-get-todo-state) "TODO")
         (let* ((heading (org-get-heading t t t t))
                (line (line-number-at-pos))
                (marker (copy-marker (point) t))
                (display (format "%s (line %d)" heading line)))
           (push (cons display marker) candidates)))))
    (nreverse candidates)))

;;;###autoload
(defun +org-project--prompt-todo (project-file)
  "Prompt for a TODO heading in PROJECT-FILE and return its marker."
  (with-current-buffer (find-file-noselect project-file)
    (let ((candidates (+org-project--todo-candidates)))
      (unless candidates
        (user-error "No TODO headings found in %s" project-file))
      (cdr (assoc (completing-read "Select TODO: " candidates nil t)
                  candidates)))))

;;;###autoload
(defun +org-project--goto-done-date-heading ()
  "Move point to today's date heading under `* Done', creating headings as needed."
  (let ((date (format-time-string "%-m/%-d/%Y")))
    (widen)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Done[ \t]*$" nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* Done\n"))
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
      (save-restriction
        (narrow-to-region (point) subtree-end)
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\*\\* %s[ \t]*$"
                                           (regexp-quote date))
                                   nil t)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "** %s\n" date)))))
    (org-back-to-heading t)))

;;;###autoload
(defun +org-project--shift-subtree-levels (text delta)
  "Shift every Org heading in TEXT by DELTA levels."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)" nil t)
      (replace-match (make-string (max 1 (+ (length (match-string 1)) delta))
                                  ?*)
                     t t nil 1))
    (buffer-string)))

;;;###autoload
(defun +org-project--move-heading-to-done (marker)
  "Move heading at MARKER under today's `* Done' date heading."
  (let (subtree source-level)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading t)
       (setq source-level (org-outline-level))
       (org-todo "DONE")
       (let ((start (point))
             (end (save-excursion (org-end-of-subtree t t))))
         (setq subtree (buffer-substring start end))
         (delete-region start end))
       (+org-project--goto-done-date-heading)
       (org-end-of-subtree t t)
       (unless (bolp) (insert "\n"))
       (insert (+org-project--shift-subtree-levels subtree (- 3 source-level)))
       (unless (bolp) (insert "\n"))
       (save-buffer)))))

;;;###autoload
(defun +org-project-add-todo ()
  "Capture a TODO under the Tasks heading of a selected project file."
  (interactive)
  (let* ((project-file (+org-project--prompt-file))
         (org-capture-templates
          `(("p" "project todo" entry
             (file+headline ,project-file "Tasks")
             "** TODO %?"
             :create-heading t))))
    (org-capture nil "p")))

;;;###autoload
(defun +org-project--goto-todays-progress ()
  "Move point to today's date heading under `* Progress Log', creating headings as needed."
  (let ((date (format-time-string "%-m/%-d/%Y")))
    (widen)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Progress Log[ \t]*$" nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* Progress Log"))
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
      (save-restriction
        (narrow-to-region (point) subtree-end)
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\*\\* %s[ \t]*$"
                                           (regexp-quote date))
                                   nil t)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "** %s" date)))))
    (org-back-to-heading t)))

;;;###autoload
(defun +org-project-add-done ()
  "Capture a DONE heading under today's Done entry in a selected project file."
  (interactive)
  (let* ((project-file (+org-project--prompt-file))
         (org-capture-templates
          `(("p" "project done" entry
             (file+function ,project-file +org-project--goto-done-date-heading)
             "*** DONE %?"))))
    (org-capture nil "p")))

;;;###autoload
(defun +org-project-mark-done (&optional marker)
  "Prompt for a project TODO and move it under today's `* Done' date heading.

When MARKER is non-nil, move the heading at MARKER instead of prompting
for a TODO."
  (interactive)
  (let* ((project-file (unless marker (+org-project--prompt-file)))
         (todo-marker (or marker (+org-project--prompt-todo project-file))))
    (+org-project--move-heading-to-done todo-marker)))

;;;###autoload
(defun +org-project-mark-done-dwim ()
  "Move the heading at point to done, or prompt like `+org-project-mark-done'."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at org-heading-regexp))
      (save-excursion
        (beginning-of-line)
        (+org-project-mark-done (copy-marker (point) t)))
    (+org-project-mark-done)))

(provide '+org-project)
;;; org-project.el ends here
