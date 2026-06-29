;;; -*- lexical-binding: t -*-
;; https://karthinks.com/software/avy-can-do-anything/
(require 'embark)
(require 'avy)

;;;###autoload
(defun avy-action-embark (pt)
  "Perform an embark action on the avy target and move the point to it"
  (goto-char pt)
  (embark-act))

;;;###autoload
(defun avy-action-embark-dwim (pt)
  (goto-char pt)
  (embark-dwim))

;;;###autoload
(defun avy-action-yank-move (pt)
  (interactive)
  (save-excursion
    (goto-char pt)
    (yank)))

;;;###autoload
(defun avy-action-kill-line-move (pt)
  (goto-char pt)
  (kill-whole-line))

;;;###autoload
(defun avy-action-kill-line-stay (pt)
  (save-excursion
    (avy-action-kill-line-move pt)))

;;;###autoload
(defun avy-action-kill-whole-lines (pt)
  "Kill whole lines until pt."
  (let ((backwards (< pt (point)))
        xend)
    (if backwards (end-of-visual-line) (beginning-of-visual-line))
    (setq xend (save-excursion
                 (goto-char pt)
                 (if backwards
                     (progn
                       (end-of-visual-line))
                   (progn
                     (beginning-of-visual-line)))
                 (point)))
    (kill-region (point) xend)))

;;;###autoload
(defun avy-action-kill-to-point (pt)
  "Kill whole region from point to `pt'."
  (kill-region (point) pt))

;;;###autoload
(defun avy-action-mark-until-pt (pt)
  (if (region-active-p)
      (goto-char pt)
    (progn
      (set-mark (point))
      (goto-char pt))))

;;;###autoload
(defun avy-action-move-line-or-region-stay (pt)
  (let ((point (point)))
    (avy-action-move-line-or-region-move pt)
    (goto-char point)))

;;;###autoload
(defun avy-action-move-line-or-region-move (pt)
  "Move the current line/region to pt."
  (let* ((region-active (region-active-p))
         (full-line-region (full-line-region-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active
                  (region-end)
                (line-beginning-position 2)))
         (column (current-column)))
    (kill-region beg end)
    (goto-char pt)
    (when (or full-line-region (not region-active))
      (beginning-of-line))
    (yank)
    (previous-line)
    (forward-char column)))

;;;###autoload
(defun avy-action-move-other-line (pt)
  "Move line at point to above the current line."
  (let ((current-line (line-number-at-pos))
        (target-line (progn (goto-char pt) (line-number-at-pos))))
    (cond
     ((> target-line current-line)
      (goto-line (1+ target-line))
      (transpose-lines (- current-line target-line)))
     ((< target-line current-line)
      (goto-line (1+ target-line))
      (transpose-lines (- current-line target-line 1)))
     (t nil))))

(provide 'avy-actions)
