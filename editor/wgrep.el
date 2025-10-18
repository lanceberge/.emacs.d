;;; -*- lexical-binding: t -*-
(use-package wgrep
  :bind
  (:map grep-mode-map
        ("R" . #'+grep-wgrep-replace)
        ("i" . #'wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map
        ([remap save-buffer] . +wgrep-finish-edit)
        ("R" . #'+wgrep-replace)))

;;;###autoload
(defun +grep-wgrep-replace ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (call-interactively #'+wgrep-replace))

;;;###autoload
(defun +wgrep-finish-edit ()
  (interactive)
  (wgrep-finish-edit)
  (save-some-buffers t))

;;;###autoload
(defun +wgrep-replace (regexp replace)
  "Replace in wgrep without replacing the read-only 'file_name:line:' prefix."
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([^:]*:[0-9]+:\\)" nil t)
      (let ((prefix-end (point))
            (line-end (line-end-position)))
        (while (re-search-forward regexp (line-end-position) t)
          (replace-match replace t nil))
        (forward-line)
        (beginning-of-line)))))
