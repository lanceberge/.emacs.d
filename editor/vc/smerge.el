;;; -*- lexical-binding: t -*-
(use-package smerge-mode
  :ensure nil
  :hook
  ((prog-mode text-mode) . smerge-mode)
  :bind
  (:map smerge-basic-map
        ("n" . #'+smerge-vc-next-conflict))
  (:map +forward-map
        ("m" . #'+smerge-vc-next-conflict))
  (:map +backward-map
        ("m" . #'smerge-prev))
  :config
  (dolist (func '(smerge-keep-current smerge-keep-upper smerge-keep-lower smerge-keep-all))
    (advice-add func :after #'+save-buffer-advice))

  ;; add everything from the smerge keymap to repeat-map
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

;;;###autoload
(defun +save-buffer-advice ()
  (when (buffer-modified-p)
    (save-buffer)))

;;;###autoload
(defun +smerge-vc-next-conflict ()
  (interactive)
  (require 'project)
  (condition-case nil
      (smerge-next)
    (error
     (when (and (buffer-modified-p) buffer-file-name)
       (save-buffer))
     (let* ((root (expand-file-name (project-root (project-current t))))
            (default-directory root)
            (current (and buffer-file-name (expand-file-name buffer-file-name)))
            ;; ripgrep only jj-tracked files for the conflict start marker
            (files (split-string
                    (shell-command-to-string
                     (format "jj file list | xargs -d '\\n' rg --files-with-matches --no-messages -- %s"
                             (shell-quote-argument "^<<<<<<<")))
                    "\n" t))
            ;; absolute paths, excluding the current file so we move forward
            (files (seq-remove
                    (lambda (f) (equal f current))
                    (mapcar (lambda (f) (expand-file-name f root)) files))))
       (if (null files)
           (message "No more conflicted files")
         (find-file (car files))
         (goto-char (point-min))
         (re-search-forward "^<<<<<<<" nil t)
         (beginning-of-line)
         (message "%s more conflicted files after this one"
                  (if (cdr files) (length (cdr files)) "No")))))))
