;;; -*- lexical-binding: t -*-
;;;###autoload
(defun pipe-region (start end command)
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
  (interactive (append
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))
                (list (read-shell-command "Pipe through: "))))
  (let ((exit-status (call-shell-region start end command t t)))
    (unless (equal 0 exit-status)
      (let ((error-msg (string-trim-right (buffer-substring (mark) (point)))))
        (undo)
        (cond
         ((null exit-status)
          (message "Unknown error"))
         ((stringp exit-status)
          (message "Signal %s" exit-status))
         (t
          (message "[%d] %s" exit-status error-msg)))))))

;;;###autoload
(defun +file-name-kill-ring-save ()
  "Copy current buffer file name to the kill ring and system clipboard."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (kill-new buffer-file-name)
  (gui-set-selection 'CLIPBOARD buffer-file-name)
  (message "Copied file name: %s" buffer-file-name))

(defun text-to-clipboard ()
  "Pop up a temporary buffer to collect text to send to the clipboard.
The pop up buffer is in `markdown-mode' and uses the TeX input
method.  Use \\<text-to-clipboard-minor-mode-map>\\[text-to-clipboard--done] to send the buffer contents to the clipboard
and quit the window, killing the buffer.

If the region is active, use the region as the initial contents
for the pop up buffer."
  (interactive)
  (let ((region (when (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))
    (pop-to-buffer (generate-new-buffer "*clipboard*"))
    (when region (insert region)))
  (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
  (text-to-clipboard-minor-mode))
(defun text-to-clipboard--done ()
  "Copy buffer contents to clipboard and quit window."
  (interactive)
  (gui-set-selection
   'CLIPBOARD
   (buffer-substring-no-properties (point-min) (point-max)))
  (quit-window :kill))

(defvar-keymap text-to-clipboard-minor-mode-map
  "C-c C-c" #'text-to-clipboard--done)

(define-minor-mode text-to-clipboard-minor-mode
  "Minor mode binding a key to quit window and copy buffer to clipboard.")

;; https://stackoverflow.com/questions/2588277/how-can-i-swap-or-replace-multiple-strings-in-code-at-the-same-time
;;;###autoload
(defun parallel-replace (plist &optional start end)
  (interactive
   `(,(cl-loop with input = (read-from-minibuffer "Replace: ")
               with limit = (length input)
               for (item . index) = (read-from-string input 0)
               then (read-from-string input index)
               collect (prin1-to-string item t) until (<= limit index))
     ,@(if (use-region-p) `(,(region-beginning) ,(region-end)))))
  (let* ((alist (cl-loop for (key val . tail) on plist by #'cddr
                         collect (cons key val)))
         (matcher (regexp-opt (mapcar #'car alist) 'words)))
    (save-excursion
      (goto-char (or start (point)))
      (while (re-search-forward matcher (or end (point-max)) t)
        (replace-match (cdr (assoc-string (match-string 0) alist)))))))

(provide 'text-extras)
