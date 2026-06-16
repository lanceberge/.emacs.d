;;; -*- lexical-binding: t -*-
(use-package undo
  :ensure nil
  :custom
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  :bind
  ("C-M--" . #'undo-redo))

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
  (:repeat-map undo-repeat-map
               :exit
               ("f" . #'vundo))
  (:map +leader-map
        ("fu" . #'vundo))
  (:map vundo-mode-map
        ("q" . #'vundo-quit)))

(use-package undo-fu-session ; persistent undos
  :hook (prog-mode . undo-fu-session-mode))
