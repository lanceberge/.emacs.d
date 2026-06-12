;;; -*- lexical-binding: t -*-
(when IS-MAC
  (use-package tramp ; access remote files within emacs
    :ensure nil))

(when IS-LINUX
  (require 'tramp))

;;;###autoload
(defun +tramp-list-ssh-hosts ()
  "Return host aliases from ~/.ssh/config."
  (let ((config (expand-file-name "~/.ssh/config"))
        hosts)
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (while (re-search-forward "^Host \\(.*\\)$" nil t)
          (push (match-string 1) hosts))))
    (nreverse hosts)))

;;;###autoload
(defun +tramp-find-file ()
  "Pick a host from ~/.ssh/config and run `find-file' rooted there via TRAMP."
  (interactive)
  (let* ((host (completing-read "SSH host: " (+ssh-hosts) nil t))
         (default-directory (format "/ssh:%s:" host)))
    (call-interactively #'find-file)))
