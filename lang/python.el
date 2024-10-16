;;; -*- lexical-binding: t -*-
(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode))

(unless IS-MAC
  (use-package conda
    :hook
    (python-mode . (lambda ()
                     (when (bound-and-true-p conda-project-env-path)
                       (conda-env-activate-for-buffer))))))
