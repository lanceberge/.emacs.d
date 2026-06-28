;;; org-project.el --- Org capture helpers for tracking projects -*- lexical-binding: t -*-

;; TODO combine this with org timer

(require 'org)
(require 'org-capture)
(require 'org-refile)
(require 'project)
(require 'seq)
(require 'subr-x)

(defvar org-capture-templates)

(defgroup +org-project nil
  "Org project helpers."
  :group 'org)

(defcustom +org-project-directory "~/org/projects/"
  "Directory containing project org files."
  :type 'directory
  :group '+org-project)

(defcustom +org-project-prompt-function #'+org-project--prompt-file
  "Function used to prompt for an Org project file."
  :type 'function
  :group '+org-project)

(defcustom +org-project-obtain-project-function #'+org-project--infer-and-remember-for-current-project
  "Function used to obtain an Org project file for project commands."
  :type 'function
  :group '+org-project)

(defvar +org-project--project-files-for-dir nil
  "Alist mapping project directories to Org project files.")

;;;###autoload
(defun +org-project--prompt-file ()
  (completing-read "Select project file: "
                   (directory-files +org-project-directory t "\\.org$")
                   nil t))

;;;###autoload
(defun +org-project--infer-and-remember-for-current-project ()
  "Infer, remember, and return the Org project file for the current directory."
  (let* ((dir (+org-project--current-dir))
         (file (alist-get dir +org-project--project-files-for-dir nil nil #'equal))
         (inferred (unless file
                     (or (+org-project--current-org-project-file)
                         (+org-project--infer-project-file dir)))))
    (or file
        (+org-project--remember-project-file
         (or inferred (funcall +org-project-prompt-function))
         dir))))

;;;###autoload
(defun +org-project-reset-project-file-for-current-project ()
  "Forget the Org project file remembered for the current directory."
  (interactive)
  (let ((dir (+org-project--current-dir)))
    (setq +org-project--project-files-for-dir
          (seq-remove
           (lambda (entry)
             (equal dir (car entry)))
           +org-project--project-files-for-dir))
    (message "Reset Org project file for %s" dir)))

;;;###autoload
(defun +org-project-project-file ()
  "Prompt for and visit an Org project file."
  (interactive)
  (find-file (funcall +org-project-prompt-function)))

;;;###autoload
(defun +org-project-project-file-for-current-project ()
  "Visit the remembered Org project file for the current directory.

Prompt for and remember an Org project file when none is remembered."
  (interactive)
  (find-file (funcall +org-project-obtain-project-function)))

;;;###autoload
(defun +org-project--remember-project-file (project-file &optional dir)
  "Remember PROJECT-FILE as the Org project file for DIR."
  (let ((dir (+org-project--normalize-dir (or dir (+org-project--current-dir))))
        (project-file (expand-file-name project-file)))
    (setf (alist-get dir +org-project--project-files-for-dir nil nil #'equal)
          project-file)
    project-file))

;;;###autoload
(defun +org-project--current-dir ()
  "Return the current project root or `default-directory'."
  (+org-project--normalize-dir
   (or (when-let ((project (project-current nil)))
         (project-root project))
       default-directory)))

;;;###autoload
(defun +org-project--normalize-dir (dir)
  "Return DIR as an expanded directory name."
  (file-name-as-directory (expand-file-name dir)))

;;;###autoload
(defun +org-project--infer-project-file (dir)
  "Return the Org project file inferred from DIR, or nil."
  (let ((project-name (+org-project--sanitize-name
                       (file-name-nondirectory
                        (directory-file-name (expand-file-name dir))))))
    (when (not (string-empty-p project-name))
      (seq-find
       (lambda (file)
         (string= project-name
                  (+org-project--sanitize-name
                   (file-name-base file))))
       (directory-files +org-project-directory t "\\.org$")))))

;;;###autoload
(defun +org-project--current-org-project-file ()
  "Return the current file when it is an Org project file."
  (when-let ((file (buffer-file-name)))
    (let ((file (expand-file-name file))
          (project-dir (file-name-as-directory
                        (expand-file-name +org-project-directory))))
      (when (and (string= "org" (file-name-extension file))
                 (file-in-directory-p file project-dir))
        file))))

;;;###autoload
(defun +org-project--sanitize-name (name)
  "Return NAME stripped to lowercase ASCII alphanumeric characters."
  (replace-regexp-in-string
   "[^[:alnum:]]" ""
   (string-clean-whitespace
    (string-to-unibyte
     (replace-regexp-in-string "[^[:ascii:]]" "" (downcase name))))))

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
(defun +org-project--done-date-rfloc ()
  "Return an `org-refile' location for today's Done date heading."
  (let* ((date (format-time-string "%-m/%-d/%Y"))
         (done-rfloc (+org-project--done-rfloc))
         (date-rfloc (+org-project--child-rfloc done-rfloc date)))
    (or date-rfloc
        (org-refile-new-child done-rfloc date))))

;;;###autoload
(defun +org-project--done-rfloc ()
  "Return an `org-refile' location for the top-level Done heading."
  (let ((file (or (buffer-file-name)
                  (user-error "Current Org project buffer is not visiting a file"))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (or (catch 'done
           (org-map-entries
            (lambda ()
              (when (and (= (org-outline-level) 1)
                         (string= (org-get-heading t t t t) "Done"))
                (throw 'done (list "Done" file nil (point)))))
            nil 'file))
         (progn
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* Done\n")
           (forward-line -1)
           (list "Done" file nil (point)))))))

;;;###autoload
(defun +org-project--child-rfloc (parent-rfloc child)
  "Return an `org-refile' location for CHILD under PARENT-RFLOC."
  (let ((file (nth 1 parent-rfloc))
        (pos (nth 3 parent-rfloc)))
    (with-current-buffer (find-file-noselect file 'nowarn)
      (org-with-wide-buffer
       (goto-char pos)
       (let ((parent-level (org-outline-level)))
         (let (found)
           (org-map-entries
            (lambda ()
              (when (and (not found)
                         (= (org-outline-level) (1+ parent-level))
                         (string= (org-get-heading t t t t) child))
                (setq found (list child file nil (point)))))
            nil 'tree)
           found))))))

;;;###autoload
(defun +org-project--move-heading-to-done (marker)
  "Move heading at MARKER under today's `* Done' date heading."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-back-to-heading t)
     (org-todo "DONE")
     (let ((rfloc (save-excursion
                    (+org-project--done-date-rfloc)))
           (mark-active nil)
           (org-refile-keep nil))
       (org-refile nil nil rfloc "Mark done")
       (save-buffer)))))

;;;###autoload
(defun +org-project-add-todo (&optional project-file)
  "Capture a TODO under the Tasks heading of a selected project file."
  (interactive)
  (let* ((project-file (or project-file (funcall +org-project-prompt-function)))
         (org-capture-templates
          `(("p" "project todo" entry
             (file+headline ,project-file "Tasks")
             "** TODO %?"
             :create-heading t))))
    (org-capture nil "p")))

;;;###autoload
(defun +org-project-add-todo-for-current-project ()
  "Capture a TODO under the current project's remembered Tasks heading."
  (interactive)
  (+org-project-add-todo (funcall +org-project-obtain-project-function)))

;;;###autoload
(defun +org-project-add-done (&optional project-file)
  "Capture a DONE heading under today's Done entry in a selected project file."
  (interactive)
  (let* ((project-file (or project-file (funcall +org-project-prompt-function)))
         (org-capture-templates
          `(("p" "project done" entry
             (file+function ,project-file +org-project--goto-done-date-heading)
             "*** DONE %?"))))
    (org-capture nil "p")))

;;;###autoload
(defun +org-project-add-done-for-current-project ()
  "Capture a DONE heading under the current project's Done entry."
  (interactive)
  (+org-project-add-done (funcall +org-project-obtain-project-function)))

;;;###autoload
(defun +org-project-mark-done (&optional marker project-file)
  "Prompt for a project TODO and move it under today's `* Done' date heading.

When MARKER is non-nil, move the heading at MARKER instead of prompting
for a TODO."
  (interactive)
  (let* ((marker (or marker
                     (when (save-excursion
                             (beginning-of-line)
                             (looking-at org-heading-regexp))
                       (copy-marker (line-beginning-position) t))))
         (project-file (unless marker
                         (or project-file
                             (funcall +org-project-prompt-function))))
         (todo-marker (or marker
                          (+org-project--prompt-todo project-file))))
    (+org-project--move-heading-to-done todo-marker)))

;;;###autoload
(defun +org-project-mark-done-for-current-project ()
  "Move a TODO to done using the current project's remembered Org project file."
  (interactive)
  (let ((marker (when (save-excursion
                        (beginning-of-line)
                        (looking-at org-heading-regexp))
                  (copy-marker (line-beginning-position) t))))
    (+org-project-mark-done
     marker
     (unless marker
       (funcall +org-project-obtain-project-function)))))

;;;###autoload
(defun +org-project-delete-heading (&optional marker)
  "Delete the Org heading at MARKER or point, then save its buffer."
  (interactive)
  (with-current-buffer (or (and marker (marker-buffer marker))
                           (current-buffer))
    (org-with-wide-buffer
     (when marker
       (goto-char marker))
     (org-back-to-heading t)
     (delete-region (point) (save-excursion (org-end-of-subtree t t)))
     (save-buffer))))

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
(define-minor-mode +org-project-savehist-mode
  "Persist Org project file mappings with `savehist'."
  :global t
  (if +org-project-savehist-mode
      (add-to-list 'savehist-additional-variables
                   '+org-project--project-files-for-dir)
    (setq savehist-additional-variables
          (delq '+org-project--project-files-for-dir
                savehist-additional-variables))))

(provide 'org-project)
;;; org-project.el ends here
