;; https://thanosapollo.org/posts/use-emacs-everywhere/
(when IS-LINUX
;;;###autoload
  (defun thanos/wtype-text (text)
    "Process TEXT for wtype, handling newlines properly."
    (let* ((has-final-newline (string-match-p "\n$" text))
           (lines (split-string text "\n"))
           (last-idx (1- (length lines))))
      (string-join
       (cl-loop for line in lines
                for i from 0
                collect (cond
                         ;; Last line without final newline
                         ((and (= i last-idx) (not has-final-newline))
                          (format "wtype -s 350 \"%s\""
                                  (replace-regexp-in-string "\"" "\\\\\"" line)))
                         ;; Any other line
                         (t
                          (format "wtype -s 350 \"%s\" && wtype -k Return"
                                  (replace-regexp-in-string "\"" "\\\\\"" line)))))
       " && ")))

;;;###autoload
  (defun thanos/type ()
    "Launch a temporary frame with a clean buffer for typing."
    (interactive)
    (let ((frame (make-frame '((name . "emacs-float")
                               (fullscreen . 0)
                               (undecorated . t)
                               (width . 70)
                               (height . 20))))
          (buf (get-buffer-create "emacs-float")))
      (select-frame frame)
      (switch-to-buffer buf)
      (erase-buffer)
      (org-mode)
      (meow-insert)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
                          (propertize "C-c C-k" 'face 'help-key-binding)))
      (local-set-key (kbd "C-c C-k")
                     (lambda () (interactive)
                       (kill-new (buffer-string))
                       (delete-frame)))
      (local-set-key (kbd "C-c C-c")
                     (lambda () (interactive)
                       (start-process-shell-command
                        "wtype" nil
                        (thanos/wtype-text (buffer-string)))
                       (delete-frame))))))
