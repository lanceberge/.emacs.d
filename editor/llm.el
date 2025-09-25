;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20240620)
  (gptel-default-mode 'org-mode)
  :bind
  (:map gptel-mode-map
        ("RET" . #'gptel-send)
        ("S-<return>" . #'newline)
        ("C-c C-l" . #'+gptel-project-clear-buffer))
  :bind
  (:map +leader2-map
        ("gt" . +gptel-project)
        ("s'" . #'+gptel-project-clear-buffer))
  :config
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

;;;###autoload
(defun gptel-api-key ()
  (read-file-contents "~/secrets/claude_key"))

;;;###autoload
(defun +gptel-project-buffer-name ()
  (format "gptel-%s" (+current-proj-tab-name)))

;;;###autoload
(defun +gptel-project ()
  (interactive)
  (let* ((gptel-buffer-name (+gptel-project-buffer-name))
         (gptel-buffer (get-buffer gptel-buffer-name)))
    (if gptel-buffer
        (switch-to-buffer gptel-buffer)
      (switch-to-buffer (gptel gptel-buffer-name))))
  (meow-insert-mode))

;;;###autoload
(defun +gptel-project-clear-buffer ()
  (interactive)
  (let ((gptel-proj-buffer (+gptel-project-buffer-name)))
    (unless (eq (current-buffer) (get-buffer gptel-proj-buffer))
      (+gptel-project))
    (with-current-buffer gptel-proj-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

(use-package elysium
  :bind
  (:map +leader2-map
        ("sq" . #'elysium-query)
        ("so" . #'elysium)
        ("sc" . #'elysium-add-context)
        ("sw" . #'elysium-toggle-window))
  (:map +leader-map
        ("m SPC" . #'elysium-keep-all-suggested-changes)
        ("m-" . #'elysium-discard-all-suggested-changes)))
