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

(defun text-to-clipboard--done ()
  "Copy buffer contents to clipboard and quit window."
  (interactive)
  (kill-region (point-min) (point-max))
  (quit-window :kill))

(defvar-keymap text-to-clipboard-minor-mode-map
  "C-c C-c" #'text-to-clipboard--done)

(define-minor-mode text-to-clipboard-minor-mode
  "Minor mode binding a key to quit window and copy buffer to clipboard.")

;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/text-extras.el
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
  (text-to-clipboard-minor-mode)
  (meow-insert-mode))
