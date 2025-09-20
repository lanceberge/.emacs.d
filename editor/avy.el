;;; -*- lexical-binding: t -*-
(use-package avy
  :custom
  (avy-keys '(?j ?d  ?s ?l ?a))
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
         (cons ?k 'avy-action-kill-lines-to-point-stay)
         (cons ?, 'avy-action-embark)
         (cons ?K 'avy-action-kill-line-stay)
         (cons ?t 'avy-action-move-region))))

;; https://karthinks.com/software/avy-can-do-anything/
;;;###autoload
(defun avy-action-embark (pt)
  "Perform an embark action on the avy target and move the point to it"
  (goto-char pt)
  (embark-act))

;;;###autoload
(defun avy-action-kill-line-stay (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line)))

;;;###autoload
(defun avy-action-kill-lines-to-point-stay (pt)
  "Kill from current point to the match point PT, excluding the match."
  (save-excursion
    (kill-region (point) (progn (goto-char pt) (end-of-visual-line) (point)))))

;;;###autoload
(defun avy-action-mark-until-pt (pt)
  (if (region-active-p)
      (goto-char pt)
    (progn
      (set-mark (point))
      (goto-char pt))))

;;;###autoload
(defun avy-action-move-region (pt)
  (let* ((region-active (region-active-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-beginning-position 2))))
    (kill-region beg end)
    (goto-char pt)
    (unless region-active
      (beginning-of-visual-line))
    (yank)))

;;;###autoload
(defun +avy-goto-char-2-below (char1 char2)
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)))
  (forward-char 1)
  (search-forward  (concat (char-to-string char1) (char-to-string char2)))
  (backward-char 2)
  (avy-goto-char-2
   char1 char2 nil
   (point) (window-end (selected-window) t)))

;;;###autoload
(defun +avy-goto-char-2-above (char1 char2)
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)))
  (backward-char 1)
  (search-backward (concat (char-to-string char1) (char-to-string char2)))
  (forward-char 2)
  (avy-goto-char-2-above
   char1 char2))
