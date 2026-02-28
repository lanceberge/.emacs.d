;;; -*- lexical-binding: t -*-
(use-package +increment-number
  :ensure nil
  :bind
  (:map +normal-mode-map
        ("M-`" . #'+increment-number-increment)
        ("M-~" . #'+increment-number-decrement)))

;;;###autoload
(defun +increment-number--number-at-point-bounds ()
  (save-excursion
    (let ((original-point (point))
          (found-bounds))
      (while
          (let ((char (char-after (point))))
            (if (and (>= char ?0) (<= char ?9))
                (backward-char))))
      (if (eq (char-before (point)) ?-)
          (backward-char))
      (re-search-forward "\\([-]?[0-9]+\\(?:\\.[0-9]*\\)?\\)" (line-end-position) t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (setq found-bounds (cons match-start match-end)))
      found-bounds)))

;;;###autoload
(defun +increment-number--number-at-point ()
  (let ((bounds (+increment-number--number-at-point-bounds)))
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (number-str (buffer-substring-no-properties start end))
             (number (string-to-number number-str)))
        (list number start end)))))

;;;###autoload
(defun +increment-number-increment (increment)
  (interactive "P")
  (let ((orig-point (point)))
    (if (re-search-forward "\\([-]?[0-9]+\\(?:\\.[0-9]*\\)?\\)" (line-end-position) t)
        (goto-char (match-beginning 0))
      (goto-char orig-point)
      (user-error "no number found"))
    (let ()
      (let* ((increment (if increment
                            (let ((val (prefix-numeric-value increment)))
                              (if (zerop val) 1 val))
                          1))
             (number-at-point (+increment-number--number-at-point))
             (current-number (nth 0 number-at-point))
             (point (point))
             (start (nth 1 number-at-point))
             (end (nth 2 number-at-point))
             (new-number (+ current-number increment))
             (old-str (buffer-substring-no-properties start end))
             (new-str (if (and (integerp current-number) (integerp increment))
                          (number-to-string new-number)
                        (format "%g" new-number))))
        (delete-region start end)
        (insert new-str)
        ;; if the size of the number decreased and we're at the end of the number, go left. Otherwise stay
        (if (and (< (length new-str) (length old-str))
                 (eq (+ 1 point) end))
            (backward-char)
          (goto-char point))))))

;;;###autoload
(defun +increment-number-decrement (decrement)
  (interactive "p")
  (let ((decrement (or decrement 1)))
    (+increment-number-increment (- decrement))))
