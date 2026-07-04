;;; -*- lexical-binding: t -*-

(require 'puni)

(defgroup +puni nil
  "Extensions for puni."
  :group 'editing)

(defcustom +puni-extras-default-kill-region-command #'kill-region
  "Command used by puni extras commands to kill the active region."
  :type 'function
  :group '+puni)

;;;###autoload
(defun +puni-kill-region-or-outer-sexp (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively +puni-extras-default-kill-region-command)
    (dotimes (_ arg)
      (puni-mark-sexp-at-point))
    (call-interactively +puni-extras-default-kill-region-command)))

;;;###autoload
(defun +puni-kill-region-or-inner-sexp (arg)
  "If there is a region, kill it.
Otherwise mark the inner block of the sexp at point and kill it."
  (interactive "p")
  (if (region-active-p)
      (call-interactively +puni-extras-default-kill-region-command)
    (dotimes (_ arg)
      (puni-mark-sexp-at-point))
    (call-interactively +puni-extras-default-kill-region-command)))

;;;###autoload
(defun +puni-soft-kill-region ()
  "Kill balanced expressions within the active region."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((final-point (min (point) (mark))))
    (unless (puni-soft-delete
             (region-beginning)
             (region-end)
             'strict-sexp
             'beyond
             'kill)
      (user-error "No balanced expression to kill"))
    (goto-char final-point)))

;;;###autoload
(defun +puni-kill-line-dwim (arg col)
  "Kill `ARG' whole lines or run `kill-line' if no arg is provided"
  (interactive (list
                current-prefix-arg
                (current-column)))
  (if arg
      (progn
        (beginning-of-line)
        (puni-kill-line arg)
        (+puni--join-leftover-closing-delimiters)
        (move-to-column col))
    (puni-kill-line)
    (+puni--join-leftover-closing-delimiters)))

;;;###autoload
(defun +puni--join-leftover-closing-delimiters ()
  "Join a line containing only closing delimiters onto the previous line."
  (when (save-excursion
          (beginning-of-line)
          (looking-at-p "\\s-*\\s)+\\s-*$"))
    (delete-indentation)
    (delete-horizontal-space)))

(provide 'puni-extras)
