;;; -*- lexical-binding: t -*-
(require 'gptel)
(require 'editor-lisp)

;;;###autoload
(defun +gptel-rewrite-directive-from-agents-md ()
  "Return the contents of the current project's AGENTS.md, if any."
  (when-let* ((root (ignore-errors (+project--current-proj-name)))
              (agents-file (expand-file-name "AGENTS.md" root))
              ((file-exists-p agents-file)))
    (with-temp-buffer
      (insert-file-contents agents-file)
      (buffer-string))))

;;;###autoload
(defun +gptel-send ()
  (interactive)
  (goto-char (point-max))
  (gptel-send))

;;;###autoload
(defun gptel-api-key ()
  (base64-decode-string
   (read-file-contents "~/secrets/claude_key")))

;;;###autoload
(defun +gptel-project-buffer-name ()
  (format "gptel-%s" (+project--current-proj-name)))

;;;###autoload
(defun +gptel-project ()
  (interactive)
  (let* ((gptel-buffer-name (+gptel-project-buffer-name))
         (gptel-buffer (get-buffer gptel-buffer-name)))
    (if gptel-buffer
        (switch-to-buffer gptel-buffer)
      (switch-to-buffer (gptel gptel-buffer-name)))))

;;;###autoload
(defun +gptel-project-clear-buffer ()
  (interactive)
  (let ((gptel-proj-buffer (+gptel-project-buffer-name)))
    (unless (eq (current-buffer) (get-buffer gptel-proj-buffer))
      (+gptel-project))
    (with-current-buffer gptel-proj-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

;;;###autoload
(defun +gptel-project-add-context ()
  (interactive)
  (let ((content (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
        (code-buffer-language
         (string-trim-right
          (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (+gptel-project)
    (with-current-buffer (+gptel-project-buffer-name)
      (goto-char (point-max))
      (insert "\n")
      (let ((src-pattern
             (cond
              ((derived-mode-p 'markdown-mode)
               "```%s\n%s\n```")
              ((derived-mode-p 'org-mode)
               "#+begin_src %s\n%s\n#+end_src")
              (t "%s%s"))))
        (insert (format src-pattern code-buffer-language content))))))

(provide 'gptel-extras)
