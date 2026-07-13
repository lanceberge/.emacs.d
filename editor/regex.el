;;; -*- lexical-binding: t -*-
(use-package visual-regexp
  :bind
  (:map +normal-mode-map
        ("%" . +replace)))

;;;###autoload
(defun +replace ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (call-interactively #'vr/replace)
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'vr/replace)))))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/command-python
        (replace-regexp-in-string "python " "python3 " vr/command-python)))
