;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20240620)
  (gptel-default-mode 'org-mode)
  :bind
  (:map gptel-mode-map
        ("RET" . #'gptel-send)
        ("S-<return>" . #'newline))
  :bind
  (:map +leader2-map
        ("gt" . +gptel-project))
  :config
  (defun gptel-api-key ()
    (read-file-contents "~/secrets/claude_key"))
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(defun +gptel-project-buffer-name ()
  (format "gptel-%s" (+current-proj-tab-name)))

(defun +gptel-project ()
  (interactive)
  (let* ((gptel-buffer-name (+gptel-project-buffer-name))
         (gptel-buffer (get-buffer gptel-buffer-name)))
    (if gptel-buffer
        (switch-to-buffer gptel-buffer)
      (switch-to-buffer (gptel (format "gptel-%s" gptel-buffer-name)))))
  (meow-insert-mode))

(use-package elysium
  :bind
  (:map +leader2-map
        ("sq" . #'elysium-query)
        ("so" . #'elysium)
        ("sc" . #'elysium-add-context)
        ("sw" . #'elysium-toggle-window)
        ("s'" . #'elysium-clear-buffer))
  (:map +leader-map
        ("m SPC" . #'elysium-keep-all-suggested-changes)
        ("m-" . #'elysium-discard-all-suggested-changes)))
