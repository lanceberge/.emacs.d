;;; -*- lexical-binding: t -*-
(use-package +vc
  :ensure nil
  :bind
  (:map +leader2-map
        ("gf" . #'+vc-modified-files)))

(defun +vc--modified-files-command (root)
  "Return the shell command listing modified files in ROOT."
  (if (file-directory-p (expand-file-name ".jj" root))
      "jj diff --name-only"
    "git status --porcelain | grep -E '^[AM][ M]?|^[ M][ M]|^\\?\\?' | awk '{print $2}'"))

;;;###autoload
(defun +vc-modified-files ()
  "Pick a modified file in the current project (jj or git) with consult preview."
  (interactive)
  (let* ((root (expand-file-name (project-root (project-current t))))
         (default-directory root)
         (output (shell-command-to-string (+vc--modified-files-command root)))
         (files (split-string (string-trim output) "\n" t)))
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
