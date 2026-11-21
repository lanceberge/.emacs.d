;;; -*- lexical-binding: t -*-
(use-package kubed
  :init
  (+modal-define-intersection-mode kubed-pods-mode +motion-mode)
  :bind
  (:map +leader-map
        ("kp" . #'kubed-list-pods))
  (:map +modal-kubed-pods-motion-mode-map
        ("x" . #'kubed-list-delete-marked)))
