;;; -*- lexical-binding: t -*-
(use-package avy
  :custom
  (avy-keys '(?j ?d  ?s ?l ?a ?g ?h ?e ?i ?c ?n))
  (avy-single-candidate-jump nil)
  (avy-case-fold-search nil)
  :bind
  (:map meow-normal-state-keymap
        ("v" . #'avy-goto-char-2))
  :config
  (setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-char-2-below . avy-order-closest)
                           (avy-goto-char-2-above . avy-order-closest)))

  (setq avy-dispatch-alist
        (list
         (cons ?m 'avy-action-mark-until-pt)
         (cons ?s 'avy-action-kill-to-point)
         (cons ?, 'avy-action-embark)
         (cons ?' 'avy-action-embark-dwim)

         (cons ?p 'avy-action-yank-move)

         (cons ?k 'avy-action-kill-line-stay)
         (cons ?K 'avy-action-kill-line-move)

         (cons ?t 'avy-action-move-line-or-region-stay)
         (cons ?T 'avy-action-move-line-or-region-move)

         (cons ?x 'avy-action-kill-whole-lines))))
;; TODO (cons ?T 'avy-action-move-other-line)

;; https://karthinks.com/software/avy-can-do-anything/
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
    (meow-yank)))

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
