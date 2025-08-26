;;; -*- lexical-binding: t -*-
(use-package prog-mode
  :ensure nil
  :bind
  (:map prog-mode-map
        ("M-;" . #'comment-line)))
