;;; -*- lexical-binding: t -*-
;;;###autoload
;; TODO hook to renaming .ex files
;; TODO rename functional component function names as well
;; TODO use xref for this
(defun +elixir-rename-module ()
  (interactive)
  (let* ((current-module-name (+elixir--current-module-name))
         (updated-module-name (+elixir--module-name-from-file))
         (current-module-base (car (last (split-string current-module-name "\\." t))))
         (updated-module-base (car (last (split-string updated-module-name "\\." t)))))
    (when (buffer-modified-p)
      (save-buffer))
    (unless (string-equal current-module-name updated-module-name)
      (+project-replace-regex
       (concat "\\b" (regexp-quote current-module-name) "\\b")
       updated-module-name)
      "*.ex")
    ;; Replace struct names
    (when (and (not (string-equal current-module-name updated-module-name))
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^[[:space:]]*defstruct" nil t)))
      (+project-replace-regex
       (concat "\\b" current-module-base ".")
       (concat updated-module-base ".")
       "*.ex")
      (+project-replace-regex
       (concat "%" current-module-base "{")
       (concat "%" updated-module-base "{")
       "*.ex"))
    (revert-buffer nil t t)))

;;;###autoload
(defun +elixir-format-buffer ()
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((buffer (current-buffer))
         (file buffer-file-name)
         (tick (buffer-chars-modified-tick))
         (output (generate-new-buffer " *mix format*"))
         (default-directory (or (locate-dominating-file file ".formatter.exs")
                                default-directory))
         (process-environment (cons "MIX_QUIET=1" process-environment)))
    (make-process
     :name "mix format"
     :buffer output
     :command (list "mix" "format" file)
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (unwind-protect
             (if (zerop (process-exit-status process))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (if (= tick (buffer-chars-modified-tick))
                         (revert-buffer :ignore-auto :noconfirm)
                       (message "mix format finished; buffer changed, not reverting"))))
               (display-buffer output))
           (when (and (zerop (process-exit-status process))
                      (buffer-live-p output))
             (kill-buffer output))))))))

(provide 'elixir-extras)
