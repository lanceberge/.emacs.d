;;; VC navigation helpers with consult previews -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)

;;;###autoload
(defun +vc-modified-files ()
  "Pick a modified file in the current project (jj or git) with consult preview."
  (interactive)
  (let* ((root (expand-file-name (project-root (project-current t))))
         (default-directory root)
         (files (+vc--modified-files root)))
    (if (null files)
        (message "No modified, new, or staged files found.")
      (let* ((absolute (mapcar (lambda (f) (expand-file-name f root)) files))
             (selected (consult--read
                        absolute
                        :prompt "Modified files: "
                        :category 'file
                        :require-match t
                        :sort nil
                        :state (consult--file-preview)
                        :history 'file-name-history)))
        (when selected
          (find-file selected))))))

;;;###autoload
(defun +vc-modified-hunks ()
  "Pick a modified hunk in the current project (jj or git) with consult preview."
  (interactive)
  (let* ((root (expand-file-name (project-root (project-current t))))
         (default-directory root)
         (output (shell-command-to-string (+vc--modified-hunks root)))
         (hunks (+vc--parse-hunks output))
         (candidates (delq nil
                           (mapcar (pcase-lambda (`(,f ,l ,c))
                                     (+vc--hunk-candidate f l c root))
                                   hunks))))
    (if (null candidates)
        (message "No modified hunks found.")
      (consult--read
       candidates
       :prompt "Modified hunks: "
       :category 'consult-location
       :require-match t
       :sort nil
       :lookup #'consult--lookup-location
       :history '(:input consult--line-history)
       :state (consult--location-state candidates)))))

(defun +vc--modified-hunks (root)
  "Return the shell command emitting a unified diff of modifications in ROOT."
  (if (file-directory-p (expand-file-name ".jj" root))
      "jj diff --git"
    "git diff HEAD"))

(defun +vc--parse-hunks (diff-output)
  "Parse unified DIFF-OUTPUT into a list of (FILE LINE CONTEXT) entries.
FILE is the new-side path, LINE is the first actually-changed new-side
line in the hunk, and CONTEXT is the trailing text from the @@ marker."
  (let ((hunks '())
        (pending nil)
        (cursor 0)
        (current-file nil))
    (cl-flet ((flush ()
                (when pending
                  (push pending hunks)
                  (setq pending nil))))
      (dolist (line (split-string diff-output "\n"))
        (cond
         ((string-match "\\`\\+\\+\\+ \\(?:b/\\)?\\(.*\\)\\'" line)
          (flush)
          (let ((path (match-string 1 line)))
            (setq current-file (and (not (string= path "/dev/null")) path))))
         ((and current-file
               (string-match
                "\\`@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@\\(.*\\)\\'"
                line))
          (flush)
          (setq cursor (max 1 (string-to-number (match-string 1 line))))
          (setq pending (list current-file cursor (string-trim (match-string 2 line)))))
         ((and pending (string-prefix-p "+" line))
          (setcar (cdr pending) cursor)
          (push pending hunks)
          (setq pending nil)
          (cl-incf cursor))
         ((and pending (string-prefix-p "-" line))
          (setcar (cdr pending) cursor)
          (push pending hunks)
          (setq pending nil))
         ((and pending (string-prefix-p " " line))
          (cl-incf cursor))))
      (flush))
    (nreverse hunks)))

(defun +vc--hunk-candidate (file line context root)
  "Build a `consult-location' candidate for hunk at FILE:LINE under ROOT.
CONTEXT is the trailing text from the diff @@ marker."
  (let* ((abs (expand-file-name file root))
         (buf (or (get-file-buffer abs)
                  (and (file-readable-p abs) (find-file-noselect abs t)))))
    (when buf
      (let* ((rel (file-relative-name abs root))
             (snippet (with-current-buffer buf
                        (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-min))
                            (forward-line (1- line))
                            (buffer-substring (pos-bol) (pos-eol))))))
             (pos (with-current-buffer buf
                    (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (pos-bol)))))
             (suffix (if (string-empty-p context)
                         snippet
                       (concat (propertize context 'face 'shadow)
                               (if (string-empty-p snippet) "" "  ")
                               snippet)))
             (display (consult--format-file-line-match rel line suffix)))
        (consult--location-candidate display (cons buf pos) line line)))))

(defun +vc--modified-files (root)
  "Return a list of modified, added, and untracked files in ROOT.
Uses jj when ROOT is a jj repo, otherwise magit."
  (let ((default-directory root))
    (if (file-directory-p (expand-file-name ".jj" root))
        (vc-jj--process-lines nil "diff" "--name-only")
      (delete-dups
       (append (magit-unstaged-files)
               (magit-staged-files)
               (magit-untracked-files))))))

(provide 'consult-vc)
