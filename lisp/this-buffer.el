;;  this-buffer.el --- Actions for this buffer   -*- lexical-binding: t; -*-
;; https://github.com/oantolin/emacs-config/blob/4d19cbcf2bfa72f8a362243b40ce4a12b7a71b69/experiments/embark-this-buffer.el

(require 'ace-window)
(require 'editor-lisp)

;;;###autoload
(defun +bookmark-file ()
  (interactive)
  (when (buffer-file-name)
    (bookmark-set (file-name-nondirectory (buffer-file-name)) nil)))

;;;###autoload
(defun +this-buffer-move-to-window ()
  "Move the current buffer to an ace-selected window."
  (interactive)
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+this-buffer--ensure-dispatch-window)
    (aw-move-window (aw-select " Ace - Move Buffer"))))

;;;###autoload
(defun +this-buffer-open-in-new-window ()
  "Open the current buffer in a newly-created window."
  (interactive)
  (let ((buffer (current-buffer)))
    (select-window (+window-split-new))
    (switch-to-buffer buffer)))

;;;###autoload
(defun +this-buffer-open-externally ()
  "Open the current buffer's file externally."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (browse-url-of-file file)
    (user-error "Current buffer is not visiting a file")))

;;;###autoload
(defun +this-buffer-shell-command-on-buffer (command)
  "Run shell COMMAND on the whole buffer."
  (interactive (list (read-shell-command "Shell command on buffer: ")))
  (shell-command-on-region (point-min) (point-max) command))

;;;###autoload
(defun +this-buffer--ensure-dispatch-window ()
  "Create a second window when ace-window would have no target choice."
  (when (one-window-p)
    (or (split-window-sensibly)
        (split-window-right))))

;;;###autoload
(defun +this-buffer-diff-with-file ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(provide 'this-buffer)
