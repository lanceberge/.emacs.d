;;; -*- lexical-binding: t -*-(use-package drag-stuff
(use-package drag-stuff
  :bind
  (:map meow-normal-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left)
        ("M-l" . #'+drag-stuff-right)
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down))
  (:map meow-insert-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left)
        ("M-l" . #'+drag-stuff-right)
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down)))

(defun +drag-stuff--horizontal (&optional arg left)
  (let ((region-active (region-active-p))
        (drag-stuff-function (if left #'drag-stuff-left #'drag-stuff-right)))
    (unless region-active
      (set-mark (point))
      (forward-char)
      (activate-mark))
    (funcall drag-stuff-function arg)
    (unless region-active
      (deactivate-mark)
      (backward-char))))

(defun +drag-stuff-left (arg)
  (interactive "p")
  (+drag-stuff--horizontal arg t))

(defun +drag-stuff-right (arg)
  (interactive "p")
  (+drag-stuff--horizontal arg nil))

;;;###autoload
(defun goto-start-of-region ()
  (if (region-active-p)
      (if (> (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

(defun goto-end-of-region ()
  (if (region-active-p)
      (if (< (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;;;###autoload
(defun +drag-stuff--word (&optional arg left)
  "Drag region one word right or left if `left' is set"
  (let ((orig-point-at-beginning (eq (point) (region-beginning)))
        (orig-region-active (region-active-p)))
    (unless orig-region-active (meow-mark-word 1))
    (if left
        (goto-start-of-region)
      (goto-end-of-region))
    (let ((move-word-function (if left #'backward-word #'forward-word))
          (drag-stuff-function (if left #'drag-stuff-region-left #'drag-stuff-region-right)))
      (dotimes (_ arg)
        (let ((current-point (point))
              (moved-word-point (save-excursion (funcall move-word-function) (point))))
          (funcall drag-stuff-function (abs (- current-point moved-word-point))))))
    ;; restore point to beginning/end of the region
    (if orig-point-at-beginning
        (when (> (point (region-beginning))
                 (exchange-point-and-mark)))
      (when (< (point) (region-end))
        (exchange-point-and-mark)))))

;;;###autoload
(defun +drag-stuff-word-left (&optional arg)
  (interactive "p")
  (+drag-stuff--word arg t))

;;;###autoload
(defun +drag-stuff-word-right (&optional arg)
  (interactive "p")
  (+drag-stuff--word arg nil))
