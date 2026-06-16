;;; -*- lexical-binding: t -*-
(use-package wgrep
  :bind
  (:map grep-mode-map
        ("%" . #'+grep-wgrep-replace)
        ("i" . #'wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map
        ([remap save-buffer] . +wgrep-finish-edit)
        ([remap quit-window] . #'+wgrep-finish-edit)
        ("%" . #'+wgrep-replace)))

(with-eval-after-load 'xref
  (define-key xref--xref-buffer-mode-map (kbd "E") #'+xref-export-to-grep))

;;TODO remove this in emacs 31 where it's built in
;;;###autoload
(defun +xref-export-to-grep ()
  "Export Xref results to a grep-mode buffer."
  (interactive)
  (unless (derived-mode-p 'xref--xref-buffer-mode)
    (user-error "Not in an Xref buffer"))
  (let* ((xrefs (or (and (boundp 'xref--fetcher)
                         xref--fetcher
                         (funcall xref--fetcher))
                    (user-error "No xref items found")))
         (xref-default-directory default-directory)
         (buffer (get-buffer-create "*xref export - grep*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: grep; default-directory: %S -*-\n\n"
                        xref-default-directory))
        (dolist (xref xrefs)
          (let* ((location (xref-item-location xref))
                 (group (xref-location-group location))
                 (line (xref-location-line location))
                 (summary (xref-item-summary xref)))
            (when (and group line)
              (insert (format "%s:%d:%s\n" group line summary))))))
      (grep-mode)
      (setq default-directory xref-default-directory))
    (pop-to-buffer buffer)))

;;;###autoload
(defun +grep-wgrep-replace ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (call-interactively #'+wgrep-replace))

;;;###autoload
(defun +wgrep-finish-edit ()
  (interactive)
  (wgrep-finish-edit)
  (save-some-buffers t))

;;;###autoload
(defun +wgrep-replace (regexp replace)
  "Replace in wgrep without replacing the read-only 'file_name:line:' prefix."
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([^:]*:[0-9]+:\\)" nil t)
      (let ((prefix-end (point))
            (line-end (line-end-position)))
        (while (re-search-forward regexp (line-end-position) t)
          (replace-match replace t nil))
        (forward-line)
        (beginning-of-line)))))
