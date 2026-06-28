;;; -*- lexical-binding: t -*-

(require 'puni)

(defgroup +puni nil
  "Extensions for puni."
  :group 'editing)

(defcustom +puni-open-chars '(?\( ?\{ ?\[ ?\")
  "Opening delimiter characters handled by puni extensions."
  :type '(repeat character))

(defcustom +puni-close-chars '(?\) ?\} ?\] ?\")
  "Closing delimiter characters handled by puni extensions."
  :type '(repeat character))

;;;###autoload
(defun +puni-slurp-or-barf-left (&optional N)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-char 1))
  (let ((char-at-point (char-after (point))))
    (cond
     ((member char-at-point +puni-open-chars)
      (dotimes (_ N)
        (forward-char)
        (puni-slurp-backward)
        (if (char-equal char-at-point ?\()
            (progn
              (backward-sexp)
              (backward-char))
          (search-backward (char-to-string char-at-point)))))
     ((member char-at-point +puni-close-chars)
      (puni-barf-forward N))
     (t
      (let ((orig-point (point)))
        (skip-chars-forward (concat "^" (+puni--all-chars)))
        (if (member (char-after (point)) (append +puni-open-chars +puni-close-chars))
            (+puni-slurp-or-barf-left N)
          (goto-char orig-point))))))
  (deactivate-mark t))

;;;###autoload
(defun +puni-slurp-or-barf-right (&optional N)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-char 1))
  (let ((char-at-point (char-after (point))))
    (cond
     ((member char-at-point +puni-open-chars)
      (dotimes (_ N)
        (forward-char)
        (puni-barf-backward)
        (backward-char)))
     ((member char-at-point +puni-close-chars)
      (dotimes (_ N)
        (puni-slurp-forward)
        (message (char-to-string char-at-point))
        (if (char-equal char-at-point ?\))
            (forward-sexp)
          (search-forward (char-to-string char-at-point))
          (backward-char))))
     (t
      (let ((orig-point (point)))
        (skip-chars-forward (concat "^" (+puni--all-chars)))
        (if (member (char-after (point)) (append +puni-open-chars +puni-close-chars))
            (+puni-slurp-or-barf-right N)
          (goto-char orig-point))))))
  (deactivate-mark))

;;;###autoload
(defun +puni--all-chars ()
  (concat (apply #'string +puni-open-chars)
          (apply #'string +puni-close-chars)))

(provide 'puni-extensions)
