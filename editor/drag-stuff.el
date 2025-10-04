;;; -*- lexical-binding: t -*-
(use-package drag-stuff
  :bind
  (:map meow-normal-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left-dwim)
        ("M-l" . #'+drag-stuff-right-dwim)
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down))
  (:map meow-insert-state-keymap
        ("M-H" . #'+drag-stuff-word-left)
        ("M-L" . #'+drag-stuff-word-right)
        ("M-h" . #'+drag-stuff-left-dwim)
        ("M-l" . #'+drag-stuff-right-dwim)
        ("M-k" . #'drag-stuff-up)
        ("M-j" . #'drag-stuff-down)))

;;;###autoload
(defun full-line-region-p ()
  (and (save-excursion (goto-char (region-beginning)) (bolp))
       (save-excursion (goto-char (region-end)) (eolp))))

;;;###autoload
(defun +drag-stuff-left-dwim (arg)
  (interactive "p")
  (let ((deactivate-mark nil))
    (if (full-line-region-p)
        (save-excursion
          (let ((start-line (line-number-at-pos (region-beginning)))
                (end-line (line-number-at-pos (1- (region-end)))))
            (goto-line start-line)
            (while (<= (line-number-at-pos) end-line)
              (beginning-of-visual-line)
              (let ((whitespace-count (save-excursion (skip-chars-forward " \t"))))
                (delete-char (min arg whitespace-count)))
              (forward-line 1))))
      (+drag-stuff--horizontal arg #'drag-stuff-left))))

;;;###autoload
(defun +drag-stuff-right-dwim (arg)
  (interactive "p")
  (let ((deactivate-mark nil))
    (if (full-line-region-p)
        (save-excursion
          (let ((start-line (line-number-at-pos (region-beginning)))
                (end-line (line-number-at-pos (1- (region-end))))
                (spaces (make-string arg ?\s)))
            (goto-line start-line)
            (while (<= (line-number-at-pos) end-line)
              (beginning-of-visual-line)
              (insert spaces)
              (forward-line 1))))
      (+drag-stuff--horizontal arg #'drag-stuff-right))))

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
(defun goto-start-of-region ()
  (if (region-active-p)
      (if (> (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;;;###autoload
(defun goto-end-of-region ()
  (if (region-active-p)
      (if (< (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;;;###autoload
(defun +drag-stuff--word (&optional arg left)
  "Drag region one word right or left if `left' is set"
  (require 'drag-stuff)
  (let ((orig-point-at-beginning (eq (point) (region-beginning)))
        (orig-region-active (region-active-p)))
    (unless orig-region-active
      (set-mark (point))
      (forward-char)
      (activate-mark))
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
