;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model "claude-3-5-sonnet-20240620")
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
  :ensure (:host github :repo "lanceberge/elysium")
  :general
  ('visual
   "sq" #'elysium-query)
  (my-localleader-def
    "sq" #'elysium-query
    "so" #'elysium-keep-all-suggested-changes
    "sm" #'elysium-discard-all-suggested-changes
    "st" #'elysium-toggle-window)
  ('(insert normal) 'gptel-mode-map
   "C-<return>" #'elysium-query))

(use-package ace-link
  :general
  ('normal
   "g SPC l" #'(ace-link :which-key "goto link")))
