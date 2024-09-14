;;; -*- lexical-binding: t -*-
(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode) :init)

(use-package conda
  :hook
  (python-mode . conda-env-autoactivate-mode)
  (python-mode . (lambda ()
                   (when (bound-and-true-p conda-project-env-path)
                     (conda-env-activate-for-buffer)))))
