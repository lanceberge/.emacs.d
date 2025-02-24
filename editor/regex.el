;;; -*- lexical-binding: t -*-
(use-package visual-regexp
  :general
  ('visual
   "r" #'vr/replace)
  ('normal
   "R" #'+vr/replace)
  (my-leader-def
    "rp" #'+vr/replace))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/command-python
        (replace-regexp-in-string "python " "python3 " vr/command-python)))

;;;###autoload
(defun +vr/replace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively #'vr/replace)))
