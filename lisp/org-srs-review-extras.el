;;; org-srs-review-extras.el --- Review interface for Org-srs -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org-ql)
(require 'org-srs)
(require 'transient)

(defgroup +org-srs-review nil
  "Review interface extensions for Org-srs."
  :group 'org-srs)

(defcustom +org-srs-review-scope nil
  "Files or directories containing Org-srs cards."
  :type '(repeat file)
  :group '+org-srs-review)

(defvar +org-srs-review--mode-buffers nil
  "Buffers in which `+org-srs-review-mode' was enabled for a review.")

(defvar +org-srs-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'+org-srs-review-menu)
    map)
  "Keymap for `+org-srs-review-mode'.")

(cl-defstruct (+org-srs-review--source
               (:constructor +org-srs-review--source-create)
               (:copier nil))
  files)

;;;###autoload
(defun +org-srs-review-start ()
  "Start an Org-srs review session."
  (interactive)
  (call-interactively #'org-srs-review-start))

;;;###autoload
(defun +org-srs-review-filetag (&optional refresh)
  "Review cards from scoped files sharing a selected file tag.
Select All to review every file in `+org-srs-review-scope'.  With a
prefix argument REFRESH, refresh Org's file-tag metadata first."
  (interactive "P")
  (let* ((files (+org-srs-review--scope-files))
         (tags (+org-srs-review--file-tags files refresh))
         (tag (completing-read "Review group: " (cons "All" tags) nil t nil nil "All"))
         (files (if (equal tag "All")
                    files
                  (+org-srs-review--files-with-tag files tag))))
    (unless files
      (user-error "No Org files match review group: %s" tag))
    (org-srs-review-start (+org-srs-review--source-create :files files))))

;;;###autoload
(define-minor-mode +org-srs-review-mode
  "Provide local controls while reviewing Org-srs cards."
  :lighter " SRS"
  :keymap +org-srs-review-mode-map
  (if +org-srs-review-mode
      (progn
        (cl-pushnew (current-buffer) +org-srs-review--mode-buffers)
        (add-hook 'org-srs-item-after-confirm-hook #'+org-srs-review--menu-open 90 t)
        (add-hook 'org-srs-review-continue-hook #'+org-srs-review--mode-disable 40 t))
    (setq +org-srs-review--mode-buffers
          (delq (current-buffer) +org-srs-review--mode-buffers))
    (remove-hook 'org-srs-item-after-confirm-hook #'+org-srs-review--menu-open t)
    (remove-hook 'org-srs-review-continue-hook #'+org-srs-review--mode-disable t)))

;;;###autoload
(transient-define-prefix +org-srs-review-menu ()
  "Rate the current Org-srs card."
  [[("1" "Easy" org-srs-review-rate-easy)
    ("2" "Good" org-srs-review-rate-good)
    ("3" "Hard" org-srs-review-rate-hard)
    ("4" "Again" org-srs-review-rate-again)
    ("q" "Quit" org-srs-review-quit)]])

;;;###autoload
(defun +org-srs-review--menu-open (&rest _)
  "Open `+org-srs-review-menu' after revealing an Org-srs card."
  (+org-srs-review-menu))

;;;###autoload
(defun +org-srs-review--mode-enable (&rest _)
  "Enable `+org-srs-review-mode' before reviewing an Org-srs card."
  (+org-srs-review-mode +1))

;;;###autoload
(defun +org-srs-review--mode-disable (&rest _)
  "Disable `+org-srs-review-mode' after reviewing an Org-srs card."
  (+org-srs-review-mode -1))

;;;###autoload
(defun +org-srs-review--mode-disable-all ()
  "Disable `+org-srs-review-mode' in all buffers used by the review."
  (dolist (buffer (copy-sequence +org-srs-review--mode-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (+org-srs-review-mode -1)))))

;;;###autoload
(defun +org-srs-review--save-buffer ()
  "Save the current file after recording an Org-srs rating."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

;;;###autoload
(defun +org-srs-review--scope-files ()
  "Return the Org files found in `+org-srs-review-scope'."
  (delete-dups
   (cl-mapcan
    (lambda (path)
      (let ((path (expand-file-name path)))
        (cond
         ((file-directory-p path)
          (directory-files-recursively path (org-srs-query-directory-file-regexp)))
         ((file-regular-p path) (list path))
         (t (user-error "Review scope path does not exist: %s" path)))))
    +org-srs-review-scope)))

;;;###autoload
(defun +org-srs-review--file-tags (files refresh)
  "Return the file tags in FILES, refreshing Org metadata when REFRESH is non-nil."
  (sort
   (delete-dups
    (cl-mapcan
     (lambda (file)
       (with-current-buffer (find-file-noselect file)
         (when refresh
           (org-set-regexps-and-options t))
         (mapcar #'substring-no-properties org-file-tags)))
     files))
   #'string-lessp))

;;;###autoload
(defun +org-srs-review--files-with-tag (files tag)
  "Return members of FILES whose Org file tags include TAG."
  (delete-dups
   (org-ql-select files `(and (level 1) (tags-inherited ,tag))
     :action #'buffer-file-name)))

(cl-defmethod org-srs-query-function ((source +org-srs-review--source))
  "Return a query function for the files in SOURCE."
  (let ((files (+org-srs-review--source-files source)))
    (lambda (predicate)
      (cl-loop for file in files
               nconc (org-srs-query-file predicate file)))))

(add-hook 'org-srs-item-before-review-hook #'+org-srs-review--mode-enable)
(add-hook 'org-srs-review-after-rate-hook #'+org-srs-review--save-buffer)
(add-hook 'org-srs-review-finish-hook #'+org-srs-review--mode-disable-all)

(provide 'org-srs-review-extras)
;;; org-srs-review-extras.el ends here
