;;; -*- lexical-binding: t -*-
(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode))

(use-package conda
  :disabled t
  :hook
  (python-mode . (lambda ()
                   (when (bound-and-true-p conda-project-env-path)
                     (conda-env-activate-for-buffer)))))
