(defun git-modified-files-select ()
  "Display git modified files in a completion interface and return the selected file."
  (interactive)
  (let* ((default-directory (or (vc-git-root default-directory) default-directory))
         (output (shell-command-to-string "git status --porcelain | grep '^[ M][ M]' | awk '{print $2}'"))
         (files (split-string (string-trim output) "\n" t)))
    (if (null files)
        (message "No modified files found.")
      (let ((selected-file (completing-read "Select modified file: " files nil t)))
        (when selected-file
          (find-file selected-file)
          (message "Opened: %s" selected-file))))))
