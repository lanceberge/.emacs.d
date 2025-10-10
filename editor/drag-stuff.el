;;; -*- lexical-binding: t -*-
(use-package drag-stuff
  :bind
  (:repeat-map drag-stuff-vertical-repeat-map
               ("j" . #'drag-stuff-down)
               ("k" . #'drag-stuff-up))
  (:map meow-normal-state-keymap
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down))
  (:map meow-insert-state-keymap
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down)))

(use-package +drag-stuff
  :ensure nil
  :bind
  (:repeat-map drag-stuff-horizontal-repeat-map
               ("h" . #'+drag-stuff-left-dwim)
               ("l" . #'+drag-stuff-right-dwim)
               ("H" . #'+drag-stuff-word-left)
               ("L" . #'+drag-stuff-word-right))
  (:map meow-normal-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left-dwim)
        ("M-l" . #'+drag-stuff-right-dwim))
  (:map meow-insert-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left-dwim)
        ("M-l" . #'+drag-stuff-right-dwim)))

;;;###autoload
(defun full-line-region-p ()
  (and (save-excursion (goto-char (region-beginning)) (bolp))
       (save-excursion (goto-char (region-end)) (eolp))))

;;;###autoload
(defun +drag-stuff-left-dwim (arg)
  (interactive "P")
  (let ((deactivate-mark nil))
    (if (full-line-region-p)
        (+indent-left (or arg tab-width))
      (let ((arg (min (- (region-beginning) (beginning-of-indentation-position)) (or arg 1))))
        (unless (eq arg 0)
          (+drag-stuff--horizontal arg #'drag-stuff-left))))))

;;;###autoload
(defun +drag-stuff-right-dwim (arg)
  (interactive "P")
  (let ((deactivate-mark nil))
    (if (full-line-region-p)
        (+indent-right (or arg tab-width))
      (let ((arg (min (- (line-end-position) (region-end)) (or arg 1))))
        (unless (eq arg 0)
          (+drag-stuff--horizontal arg #'drag-stuff-right))))))

;;;###autoload
(defun +drag-stuff--horizontal (&optional arg drag-stuff-function)
  (let ((region-active (region-active-p)))
    (unless region-active
      (set-mark (point))
      (forward-char)
      (activate-mark))
    (funcall drag-stuff-function arg)
    (unless region-active
      (deactivate-mark)
      (backward-char))))

;;;###autoload
(defun +drag-stuff--word (&optional arg left)
  "Drag region one word right or left if `left' is set"
  (require 'drag-stuff)
  (let ((orig-point-at-beginning (eq (point) (region-beginning)))
        (orig-region-active (region-active-p))
        (move-word-point-function (if left #'+backward-word-no-wrap-point #'+forward-word-no-wrap-point))
        (drag-stuff-function (if left #'drag-stuff-region-left #'drag-stuff-region-right)))
    (unless orig-region-active
      (set-mark (point))
      (forward-char)
      (activate-mark))
    (if left
        (goto-start-of-region)
      (goto-end-of-region))
    (dotimes (_ arg)
      (let* ((current-point (point))
             (moved-word-point (funcall move-word-point-function))
             (drag-stuff-arg (abs (- current-point moved-word-point))))
        (unless (eq drag-stuff-arg 0)
          (funcall drag-stuff-function drag-stuff-arg))))
    ;; restore point to beginning/end of the region
    (if orig-point-at-beginning
        (when (> (point (region-beginning))
                 (exchange-point-and-mark)))
      (when (< (point) (region-end))
        (exchange-point-and-mark)))))

;;;###autoload
(defun +forward-word-no-wrap-point ()
  (min (save-excursion (forward-word) (point)) (line-end-position)))

;;;###autoload
(defun +backward-word-no-wrap-point ()
  (max (beginning-of-indentation-position) (save-excursion (backward-word) (point))))

;;;###autoload
(defun +drag-stuff-word-left (&optional arg)
  (interactive "p")
  (+drag-stuff--word arg t))

;;;###autoload
(defun +drag-stuff-word-right (&optional arg)
  (interactive "p")
  (+drag-stuff--word arg nil))
