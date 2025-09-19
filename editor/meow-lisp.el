;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +meow-visit (arg)
  "Read a string from minibuffer, then find and select it.

The input will be pushed into `regexp-search-ring'.  So
\\[meow-search] can be used for further searching with the same
condition.

A list of words and symbols in the current buffer will be
provided for completion.  To search for regexp instead, set
`meow-visit-sanitize-completion' to nil.  In that case,
completions will be provided in regexp form, but also covering
the words and symbols in the current buffer.

To search backward, use \\[negative-argument]."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (raw-text (meow--prompt-symbol-and-words
                    (if arg "Visit backward: " "Visit: ")
                    (point-min) (point-max) t))
         (text (replace-regexp-in-string "\\\\\_<\\|\\\\\_>" "" raw-text))
         (visit-point (meow--visit-point raw-text reverse)))
    (if visit-point
        (let* ((m (match-data))
               (marker-beg (car m))
               (marker-end (cadr m))
               (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
               (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (thread-first
            (meow--make-selection '(select . visit) beg end)
            (meow--select t))
          (meow--push-search raw-text)
          (meow--ensure-visible)
          (meow--highlight-regexp-in-buffer raw-text)
          (setq meow--dont-remove-overlay t)
          (+isearch-update-last-search text)
          (deactivate-mark))
      (message "Visit: %s failed" raw-text))))

;;;###autoload
(defun +meow-swap-grab-or-replace ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'meow-swap-grab)
    (call-interactively #'+meow-replace-char)))

;;;###autoload
(defun +meow-change ()
  (interactive)
  (if (region-active-p)
      (progn
        (meow-change)
        (indent-according-to-mode))
    (progn
      (kill-line)
      (meow-insert-mode))))

;;;###autoload
(defun +meow-replace-char (char)
  (interactive "cChar:")
  (save-excursion
    (when (and (region-active-p) (eq (point) (region-end)))
      (backward-char))
    (progn
      (delete-char 1)
      (insert-char char))))

;;;###autoload
(defun +meow-cancel-selection ()
  (interactive)
  (meow--cancel-second-selection)
  (meow-cancel-selection)
  (if (featurep 'isearch)
      (+isearch-clear-highlighting)))

;;;###autoload
(defun +meow-pop-selection ()
  (interactive)
  (if (region-active-p)
      (meow-pop-selection)
    (set-mark-command 1)))
