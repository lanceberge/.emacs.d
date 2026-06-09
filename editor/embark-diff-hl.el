;;; embark-diff-hl.el --- Embark actions for diff-hl hunks -*- lexical-binding: t; -*-

(use-package +embark-diff-hl
  :ensure nil
  :after (embark)
  :config
  (add-to-list 'embark-keymap-alist '(diff-hl-hunk . +embark-diff-hl-hunk-map))
  (add-to-list 'embark-target-finders #'+embark-diff-hl-target-hunk-at-point)

  (defvar-keymap +embark-diff-hl-hunk-map
    :doc "Keymap for actions related to diff-hl hunks"
    :parent embark-general-map
    "s" #'diff-hl-show-hunk
    "r" #'+embark-diff-hl-revert-hunk))

;;;###autoload
(defun +embark-diff-hl-revert-hunk ()
  "Revert the diff-hl hunk at point without confirmation."
  (interactive)
  (let ((diff-hl-ask-before-revert-hunk nil))
    (diff-hl-revert-hunk)))

;;;###autoload
(defun +embark-diff-hl-target-hunk-at-point ()
  "Target a diff-hl hunk at point."
  (when (bound-and-true-p diff-hl-mode)
    (when-let* ((ovl (cl-loop for o in (overlays-in (line-beginning-position)
                                                    (line-end-position))
                              when (overlay-get o 'diff-hl)
                              return o)))
      `(diff-hl-hunk ,(symbol-name (or (overlay-get ovl 'diff-hl-hunk-type)
                                       'hunk))
                     ,(overlay-start ovl) . ,(overlay-end ovl)))))

(provide '+embark-diff-hl)
