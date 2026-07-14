;;; -*- lexical-binding: t -*-
(use-package wgrep
  :bind
  (:map grep-mode-map
        ("%" . #'+grep-wgrep-replace)
        ("e" . #'wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map
        ([remap save-buffer] . +wgrep-finish-edit)
        ([remap quit-window] . #'+wgrep-finish-edit)
        ("%" . #'+replace)))

;;;###autoload
(defun +grep-wgrep-replace ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (call-interactively #'+replace))

;;;###autoload
(defun +wgrep-finish-edit ()
  (interactive)
  (wgrep-finish-edit)
  (project-save-some-buffers t)
  (+motion-mode 1))
