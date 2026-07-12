;;; -*- lexical-binding: t -*-
(use-package drag-stuff
  :bind
  (:map text-mode-map
        ("M-n" . #'+drag-stuff-down)
        ("M-p" . #'+drag-stuff-up))
  (:map prog-mode-map
        ("M-n" . #'+drag-stuff-down)
        ("M-p" . #'+drag-stuff-up)))

;;;###autoload
(defun +drag-stuff-up ()
  "Same as `drag-stuff-up' except if point is at the beginning of the next line,
and a region is active, only drag a single line. This is useful when marking full
lines using C-a C-SPC C-n. (or `+mark-whole-lines')."
  (interactive)
  (if (and (region-active-p) (bolp))
      (progn
        (backward-char 1)
        (call-interactively #'drag-stuff-up)
        (forward-char 1))
    (call-interactively #'drag-stuff-up)))

;;;###autoload
(defun +drag-stuff-down ()
  (interactive)
  (if (and (region-active-p) (bolp))
      (progn
        (backward-char 1)
        (call-interactively #'drag-stuff-down)
        (forward-char 1))
    (call-interactively #'drag-stuff-down)))
