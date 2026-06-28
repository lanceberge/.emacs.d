;;; ace-link-agent-shell.el --- Ace-link support for agent-shell -*- lexical-binding: t -*-

;;; Code:

(require 'ace-link)
(require 'seq)

;;;###autoload
(defun +ace-link-agent-shell-open ()
  "Open a visible actionable link in `agent-shell-mode' with avy."
  (interactive)
  (let ((pt (avy-with +ace-link-agent-shell-open
              (avy-process
               (mapcar #'cdr (+ace-link-agent-shell--collect))
               (avy--style-fn avy-style)))))
    (when (number-or-marker-p pt)
      (goto-char pt)
      (when-let ((action (+ace-link-agent-shell--action-at pt)))
        (call-interactively action)))))

;;;###autoload
(defun +ace-link-agent-shell--collect ()
  "Collect visible actionable link positions in `agent-shell-mode'."
  (let ((end (window-end (selected-window) t))
        candidates)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (let* ((pos (point))
               (next (next-single-char-property-change pos 'keymap nil end)))
          (when (and (get-char-property pos 'keymap)
                     (+ace-link-agent-shell--link-at-p pos))
            (push (cons (buffer-substring-no-properties pos next) pos)
                  candidates))
          (goto-char next))))
    (nreverse candidates)))

;;;###autoload
(defun +ace-link-agent-shell--action-at (position)
  "Return the RET action at POSITION."
  (key-binding (kbd "RET") nil t position))

;;;###autoload
(defun +ace-link-agent-shell--link-at-p (position)
  "Return non-nil when POSITION looks like an agent-shell link."
  (and (not (get-char-property position 'invisible))
       (or (+ace-link-agent-shell--link-face-p
            (get-char-property position 'face))
           (+ace-link-agent-shell--link-face-p
            (get-char-property position 'font-lock-face))
           (get-char-property position 'button))))

;;;###autoload
(defun +ace-link-agent-shell--link-face-p (face)
  "Return non-nil when FACE contains `link'."
  (cond
   ((eq face 'link))
   ((consp face)
    (or (memq 'link face)
        (seq-some #'+ace-link-agent-shell--link-face-p face)))))

;;;###autoload
(define-minor-mode +ace-link-agent-shell-mode
  "Enable ace-link support for `agent-shell-mode'."
  :global t
  :keymap (make-sparse-keymap)
  (if +ace-link-agent-shell-mode
      (add-to-list 'ace-link-major-mode-actions
                   '(+ace-link-agent-shell-open agent-shell-mode))
    (setq ace-link-major-mode-actions
          (remove '(+ace-link-agent-shell-open agent-shell-mode)
                  ace-link-major-mode-actions))))

(provide 'ace-link-agent-shell)
;;; ace-link-agent-shell.el ends here
