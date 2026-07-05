;;; grep-extras.el --- Extra grep buffer commands -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'compile)
(require 'embark)

;;;###autoload
(defun +grep-export-dired ()
  "Export files from the current grep buffer to a Dired buffer."
  (interactive)
  (unless (derived-mode-p 'grep-mode)
    (user-error "Current buffer is not a grep buffer"))
  (compilation--ensure-parse (point-max))
  (let ((files (+grep--buffer-files)))
    (if files
        (+grep-embark-export-dired-unique files)
      (user-error "No files in grep buffer"))))

;;;###autoload
(defun +grep-embark-export-dired-unique (files)
  "Create an Embark Dired export for FILES with a unique buffer name."
  (let ((rename-buffer-function (symbol-function 'rename-buffer)))
    (cl-letf (((symbol-function 'rename-buffer)
               (lambda (buffer-name &optional _unique)
                 (funcall rename-buffer-function buffer-name t))))
      (embark-export-dired files))))

;;;###autoload
(defun +grep--buffer-files ()
  "Return unique files represented by the current grep buffer."
  (let ((pos (point-min))
        files)
    (while (< pos (point-max))
      (let ((next (or (next-single-property-change
                       pos 'compilation-message nil (point-max))
                      (point-max))))
        (when-let ((file (+grep--message-file
                          (get-text-property pos 'compilation-message))))
          (push file files))
        (setq pos next)))
    (delete-dups (nreverse files))))

;;;###autoload
(defun +grep--message-file (message)
  "Return the source file for a grep compilation MESSAGE."
  (when (and message
             (compilation--message-p message))
    (let* ((file-struct (compilation--loc->file-struct
                         (compilation--message->loc message)))
           (file (caar file-struct))
           (directory (cadr file-struct)))
      (when file
        (expand-file-name file (or directory default-directory))))))

(provide 'grep-extras)
