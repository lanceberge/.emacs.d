;;; -*- lexical-binding: t -*-
(defvar +open-chars '(?\( ?\{ ?\[ ?\"))
(defvar +close-chars '(?\) ?\} ?\] ?\"))

(use-package puni
  :init
  (setq +all-chars (concat (apply #'string +open-chars)
                           (apply #'string +close-chars)))
  :bind (:map meow-normal-state-keymap
              ("<" . +slurp-or-barf-left)
              (">" . +slurp-or-barf-right)))

;;;###autoload
(defun +slurp-or-barf-left (&optional N)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-char 1))
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn
             (dotimes (_ N)
               (forward-char)
               (puni-slurp-backward)
               (if (char-equal char-at-point ?\()
                   (progn (backward-sexp) (backward-char))
                 (search-backward (char-to-string char-at-point))))))
          ((member char-at-point +close-chars)
           (puni-barf-forward N))
          (t
           (progn
             (let ((orig-point (point)))
               (skip-chars-forward (concat "^" +all-chars))
               (if (member (char-after (point)) (append +open-chars +close-chars))
                   (+slurp-or-barf-left N)
                 (goto-char orig-point)))))))
  (deactivate-mark t))

;;;###autoload
(defun +slurp-or-barf-right (&optional N)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-char 1))
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn
             (dotimes (_ N)
               (forward-char)
               (puni-barf-backward)
               (backward-char))))
          ((member char-at-point +close-chars)
           (progn
             (dotimes (_ N)
               (puni-slurp-forward)
               (message (char-to-string char-at-point))
               (if (char-equal char-at-point ?\))
                   (progn (forward-sexp))
                 (progn (search-forward (char-to-string char-at-point))
                        (backward-char))))))
          (t
           (progn
             (let ((orig-point (point)))
               (skip-chars-forward (concat "^" +all-chars))
               (if (member (char-after (point)) (append +open-chars +close-chars))
                   (+slurp-or-barf-right N)
                 (goto-char orig-point))))))
    (deactivate-mark)))
