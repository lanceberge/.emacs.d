;;; -*- lexical-binding: t -*-

;;;###autoload
(defun +surround (char)
  (interactive "cCharacter to surround with: ")
  (if (eq char ?d)
      (+surround-delete)
    (unless (use-region-p)
      (let ((er/try-expand-list '(er/mark-outside-quotes er/mark-outside-pairs)))
        (er/expand-region 1)))
    (let* ((open-char (cond ((eq char ?g) ?\")
                            ((eq char ?r) ?\()
                            (t char)))
           (close-char (cond ((eq char ?g) ?\")
                             ((eq char ?r) ?\))
                             ((eq char ?\() ?\))
                             ((eq char ?\{) ?\})
                             ((eq char ?\[) ?\])
                             (t char))))
      (+surround--narrow-to-non-whitespace)
      (let ((start (region-beginning))
            (end (region-end)))
        (when (> end start)
          (save-excursion
            (goto-char end)
            (insert close-char)
            (goto-char start)
            (insert open-char)))))
    (keyboard-quit)))

;;;###autoload
(defun +surround-delete ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (+surround--narrow-to-non-whitespace)
        (+surround--delete))
    (let ((er/try-expand-list '(er/mark-outside-quotes er/mark-outside-pairs)))
      (save-excursion
        (er/expand-region 1)
        (+surround--delete)))))

;;;###autoload
(defun +surround--narrow-to-non-whitespace ()
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (non-whitespace-start (save-excursion
                                     (goto-char start)
                                     (skip-chars-forward " \t\n")
                                     (point)))
             (non-whitespace-end (save-excursion
                                   (goto-char end)
                                   (skip-chars-backward " \t\n")
                                   (point))))
        (goto-char non-whitespace-start)
        (set-mark non-whitespace-end)
        (activate-mark))
    (user-error "No active region")))

;;;###autoload
(defun +surround--delete ()
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (when (> (- end start) 1)
        (goto-char end)
        (backward-char 1)
        (delete-char 1)
        (goto-char start)
        (delete-char 1)
        (keyboard-quit)))))

(define-key meow-normal-state-keymap (kbd "S") #'+surround)
