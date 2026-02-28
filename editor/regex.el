;;; -*- lexical-binding: t -*-
(use-package visual-regexp
  :bind
  (:map +normal-mode-map
        ("R" . +replace)))

;;;###autoload
(defun +replace ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (call-interactively #'vr/replace)
      (+vr/replace))))

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
