;;; -*- lexical-binding: t -*-
(use-package visual-regexp
  :general
  (my-leader-def
    "rp" #'vr/replace))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/command-python
        (replace-regexp-in-string "python" "python3" vr/command-python)))
