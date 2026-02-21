;;; -*- lexical-binding: t -*-
(use-package jinx
  ;; :disabled t
  ;; :hook ((prog-mode text-mode) . jinx-mode)
  :bind
  (:map jinx-mode-map
        ("M-=" . #'jinx-correct)))
