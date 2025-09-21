;;; -*- lexical-binding: t -*-
(use-package avy
  :custom
  (avy-keys '(?j ?d  ?s ?l ?a ?g ?h ?e ?i ?c ?n))
  (avy-single-candidate-jump nil)
  (avy-style 'de-bruijn)
  (avy-case-fold-search nil)
  :bind
  (:map meow-normal-state-keymap
        ("F" . #'avy-goto-char-2))
  :config
  (setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-char-2-below . avy-order-closest)
                           (avy-goto-char-2-above . avy-order-closest)))

  (setq avy-dispatch-alist
        (list
         (cons ?m 'avy-action-mark-until-pt)
         (cons ?k 'avy-action-kill-to-point-move)
         (cons ?K 'avy-action-kill-line-stay)
         (cons ?, 'avy-action-embark)
         (cons ?t 'avy-action-move-region)
         (cons ?T 'avy-action-move-other-line)
         (cons ?x 'avy-action-kill-whole-lines-stay)
         (cons ?X 'avy-action-kill-whole-lines-move))))

;; https://karthinks.com/software/avy-can-do-anything/
;;;###autoload
(defun avy-action-embark (pt)
  "Perform an embark action on the avy target and move the point to it"
  (goto-char pt)
  (embark-dwim))

;;;###autoload
(defun avy-action-kill-line-stay (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line)))

;;;###autoload
(defun avy-action-kill-whole-lines-move (pt)
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
(defun avy-action-kill-whole-lines-stay (pt)
  "Kill whole lines from point to `pt'."
  (save-excursion
    (avy-action-kill-whole-lines-move pt)))

;;;###autoload
(defun avy-action-kill-to-point-move (pt)
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
(defun avy-action-move-region (pt)
  "Move the current line/region to pt."
  (let* ((region-active (region-active-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-beginning-position 2))))
    (kill-region beg end)
    (goto-char pt)
    (unless region-active
      (beginning-of-visual-line))
    (yank)))

;;;###autoload
(defun avy-action-move-other-line (pt)
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
