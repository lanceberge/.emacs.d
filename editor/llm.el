;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20240620)
  (gptel-default-mode 'org-mode)
  :general
  (my-localleader-def
    "gc" #'gptel)

  ('gptel-mode-map
   "RET" #'gptel-send
   "S-<return>" #'newline)
  :config
  (defun gptel-api-key ()
    (read-file-contents "~/secrets/claude_key"))
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(use-package elysium
  :general
  ('visual
   "sq" #'elysium-query
   "sc" #'elysium-add-context)
  ('normal
   "C-l" #'elysium-clear-buffer)
  (my-localleader-def
    "sq" #'elysium-query
    "so" #'elysium-keep-all-suggested-changes
    "sm" #'elysium-discard-all-suggested-changes
    "st" #'elysium-toggle-window
    "sc" #'elysium-add-context)
  ('(insert normal) 'gptel-mode-map
   "RET" (lambda () (interactive) (end-of-buffer) (gptel-send))
   "C-<return>" #'elysium-query))

(use-package aidermacs
  :general
  (my-leader-def
    "a" #'aidermacs-transient-menu)
  :config
  (setenv (read-file-contents "~/secrets/claude_key"))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  :config
  (setq exec-path (append exec-path '("~/.local/bin/"))))
