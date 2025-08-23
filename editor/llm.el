;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20240620)
  (gptel-default-mode 'org-mode)
  :bind
  (:map gptel-mode-map
        ("RET" . #'gptel-send)
        ("S-<return>" . #'newline))
  :config
  (defun gptel-api-key ()
    (read-file-contents "~/secrets/claude_key"))
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(use-package elysium)
