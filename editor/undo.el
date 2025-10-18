;;; -*- lexical-binding: t -*-
(use-package vundo ; interactive visual tree of undos
  :bind
  (:repeat-map undo-repeat-map
               ("f" . #'vundo))
  (:map +leader-map
        ("fu" . #'vundo))
  (:map vundo-mode-map
        ([remap meow-quit] . #'vundo-quit)))

(use-package undo-fu-session ; persistent undos
  :hook (prog-mode . undo-fu-session-mode))
