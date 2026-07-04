;;; -*- lexical-binding: t -*-
(use-package undo
  :ensure nil
  :init
  ;; don't like this default repeat map at all
  (put 'undo 'repeat-map nil)
  :custom
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000)))

;;;###autoload
(defun +undo (&optional arg)
  (interactive "*P")
  (let ((deactivate-mark nil))
    (call-interactively #'undo)))

;;;###autoload
(defun +redo (&optional arg)
  (interactive "*p")
  (let ((deactivate-mark nil))
    (call-interactively #'undo-redo)))

(use-package vundo ; interactive visual tree of undos
  :bind
  (:map goto-map
        ("u" . #'vundo)))

(use-package undo-fu-session ; persistent undos
  :hook (prog-mode . undo-fu-session-mode))
