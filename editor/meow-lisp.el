;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +meow-visit (arg)
  "`meow-visit' but add the string to the isearch history."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (raw-text (meow--prompt-symbol-and-words
                    (if arg "Visit backward: " "Visit: ")
                    (point-min) (point-max) t))
         (text (replace-regexp-in-string "\\\\" "" (replace-regexp-in-string "\\\\\_<\\|\\\\\_>" "" raw-text)))
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
  (if (eq char ?\e) ;; escape to quit
      (keyboard-quit))
  (save-excursion
    (when (and (region-active-p) (eq (point) (region-end)))
      (backward-char))
    (progn
      (delete-char 1)
      (insert-char char))))

;;;###autoload
(defun +meow-cancel-selection ()
  (interactive)
  (when (featurep 'isearch)
    (save-excursion
      (lazy-highlight-cleanup)
      (isearch-exit)))
  (meow--cancel-second-selection)
  (meow-cancel-selection))

;;;###autoload
(defun +meow-pop-selection ()
  (interactive)
  (if (region-active-p)
      (meow-pop-selection)
    (set-mark-command 1)))

;;;###autoload
(defun +meow-open-below ()
  "Open a newline below and switch to INSERT state. The previous function just executed the RET
macro which made this send the query in gptel so I replaced it with newline."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'insert)
    (goto-char (line-end-position))
    (newline)
    (indent-according-to-mode)
    (setq-local meow--insert-pos (point))
    (when meow-select-on-open
      (setq-local meow--insert-activate-mark t))))

;;;###autoload
(defun +meow-yank-or-replace ()
  (interactive)
  (if (region-active-p)
      (meow-replace)
    (meow-yank)))

;;;###autoload
(defun +meow-save ()
  (interactive)
  (unless (region-active-p)
    (if (member (char-after (point)) +open-chars)
        (+expand-region 1)
      (+expand-region 2)))
  (meow-save))

;;;###autoload
(defun +join-line ()
  (interactive)
  (join-line)
  (indent-for-tab-command))

;;;###autoload
(defun +back-or-join ()
  (interactive)
  (cond ((eq last-command this-command)
         (+join-line))
        ((eq (point) (save-excursion (back-to-indentation) (point)))
         (+join-line))
        (t
         (back-to-indentation))))

;;;###autoload
(defun +kill-line-or-region (arg)
  (interactive "P")
  (if (region-active-p)
      (meow-kill)
    (save-excursion
      (beginning-of-line)
      (kill-visual-line arg))))

;;;###autoload
(defun +smart-delete ()
  (interactive)
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (kill-sexp))
          ((member char-at-point +close-chars)
           (forward-char)
           (backward-kill-sexp))
          (t
           (delete-char 1)))))

;;;###autoload
(defun +backward-kill-sexp ()
  (interactive)
  (forward-char 1)
  (backward-kill-sexp))

;;;###autoload
(defun +save-and-exit ()
  (interactive)
  (save-buffer)
  (meow-normal-mode))

;;;###autoload
(defun +delete ()
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively #'delete-region)
        (indent-for-tab-command))
    (call-interactively #'backward-delete-char-untabify)))
