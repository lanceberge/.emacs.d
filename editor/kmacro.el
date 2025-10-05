;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("M-r" . #'+kmacro-record-or-end)
        ("M-m" . #'+kmacro-execute)
        ("M-R" . #'+kmacro-record-or-end-register)
        ("M-M" . #'jump-to-register)))

(defvar +recording-register nil)

;;;###autoload
(defun +kmacro-record-or-end (arg)
  (interactive "P")
  (if defining-kbd-macro
      (let ((register (or arg +recording-register)))
        (call-interactively #'kmacro-end-macro)
        (when register
          (message "to register %d" register)
          (kmacro-to-register arg)))
    (setq +recording-register nil) ; reset recording register
    (when arg
      (setq +recording-register arg))
    (call-interactively #'kmacro-start-macro)))

;;;###autoload
(defun +kmacro-execute (&optional arg)
  (interactive "p")
  (if defining-kbd-macro
      (call-interactively #'kmacro-end-macro))
  (with-undo-amalgamate
    (if (region-active-p)
        (progn
          (call-interactively #'apply-macro-to-region-lines)
          (deactivate-mark))
      (kmacro-call-macro arg))))
