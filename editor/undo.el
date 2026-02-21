;;; -*- lexical-binding: t -*-
(use-package undo
  :ensure nil
  :bind
  ("C-M--" . #'undo-redo)
  (:repeat-map undo-repeat-map
               ("u" . #'+undo)
               ("r" . #'+redo))
  (:map meow-normal-state-keymap
        ("u" . #'+undo)
        ("z" . #'+redo)))

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
        ([remap meow-quit] . #'vundo-quit)))

(use-package undo-fu-session ; persistent undos
  :hook (prog-mode . undo-fu-session-mode))
