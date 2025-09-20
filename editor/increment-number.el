(use-package +increment-number
  :bind
  (:map meow-normal-state-keymap
        ("M-`" . #'+increment-number-increment)
        ("~" . #'+increment-number-decrement)
        ("M-~" . #'+increment-number-decrement)))

;;;###autoload
(defun +increment-number--number-at-point-bounds ()
  (save-excursion
    (let ((original-point (point)))
      (beginning-of-line)
      (let (found-bounds)
        (while (and (not found-bounds)
                    (re-search-forward "\\([-]?[0-9]+\\(?:\\.[0-9]*\\)?\\)" (line-end-position) t))
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (when (and (>= original-point match-start)
                       (<= original-point match-end))
              (setq found-bounds (cons match-start match-end)))))
        found-bounds))))

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
(defun +increment-number-increment (&optional increment)
  (interactive "P")
  (let ((increment (if increment
                       (let ((val (prefix-numeric-value increment)))
                         (if (zerop val) 1 val))
                     1))
        (number-at-point (+increment-number--number-at-point)))
    (message "%d" increment)
    (unless number-at-point
      (user-error "no number at point"))
    (let* ((current-number (nth 0 number-at-point))
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
      (goto-char (+ point (- (length new-str) (length old-str)))))))

;;;###autoload
(defun +increment-number-decrement (&optional decrement)
  (interactive "p")
  (let ((decrement (or decrement 1)))
    (+increment-number-increment (- decrement))))
