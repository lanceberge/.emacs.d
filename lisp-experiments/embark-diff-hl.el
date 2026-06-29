;;; embark-diff-hl.el --- Embark actions for diff-hl hunks -*- lexical-binding: t; -*-

(use-package embark-diff-hl
  :ensure (:type file :main "~/.emacs.d/lisp/embark-diff-hl.el")
  :after (embark diff-hl)
  :demand t)


(require 'embark)
(require 'diff-hl)

;;;###autoload
(defun +embark-diff-hl-revert-hunk ()
  "Revert the diff-hl hunk at point without confirmation."
  (interactive)
  (let ((diff-hl-ask-before-revert-hunk nil))
    (diff-hl-revert-hunk)))

;;;###autoload
(defun +embark-diff-hl-hunk-at-point-p ()
  "Return non-nil when point is in a diff-hl hunk."
  (when (bound-and-true-p diff-hl-mode)
    (cl-loop for o in (overlays-in (line-beginning-position)
                                   (line-end-position))
             thereis (overlay-get o 'diff-hl))))

;;;###autoload
(defun +embark-diff-hl-add-hunk-actions (keymap)
  "Add diff-hl hunk actions to KEYMAP when point is in a hunk."
  (if (+embark-diff-hl-hunk-at-point-p)
      (make-composed-keymap +embark-diff-hl-hunk-map keymap)
    keymap))

;;;###autoload
(defun +embark-diff-hl-target-hunk-at-point ()
  "Target a diff-hl hunk at point.

This is kept only so reloading this file can remove the old target finder."
  nil)

(defvar-keymap +embark-diff-hl-hunk-map
  :doc "Keymap for actions related to diff-hl hunks"
  "s" #'diff-hl-show-hunk
  "R" #'+embark-diff-hl-revert-hunk)

(setq embark-keymap-alist
      (assq-delete-all 'diff-hl-hunk embark-keymap-alist))

(setq embark-target-finders
      (delq #'+embark-diff-hl-target-hunk-at-point embark-target-finders))

(advice-remove #'embark--action-keymap #'+embark-diff-hl-add-hunk-actions)
(advice-add #'embark--action-keymap :filter-return #'+embark-diff-hl-add-hunk-actions)

(provide 'embark-diff-hl)
