;;; -*- lexical-binding: t -*-
(use-package prog-mode
  :ensure nil
  :bind
  (:map prog-mode-map
        ("C-/" . #'comment-line)
        ("M-/" . #'comment-line)))
