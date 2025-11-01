;;; -*- lexical-binding: t -*-
(use-package newcomment
  :ensure nil
  :bind
  (:map text-mode-map
        ("M-;" . #'+comment-dwim))
  (:map prog-mode-map
        ("M-;" . #'+comment-dwim)))

;;;###autoload
(defun +comment-dwim ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'comment-dwim)
    (call-interactively #'comment-line)))
