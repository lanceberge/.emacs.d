;;; -*- lexical-binding: t -*-
(require 'cl)

;;;###autoload
(defun read-file-contents (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;;;###autoload
(defun +save-and-kill-buffer ()
  "save and kill buffer"
  (interactive)
  (save-buffer)
  (kill-current-buffer))

;;;###autoload
(defun balanced-parens-p ()
  "Return `t' if parentheses are balanced; otherwise `nil'."
  (condition-case nil
      (progn
        (check-parens)
        t)
    (error nil)))

;; https://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring
;;;###autoload
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

;;;###autoload
(defun full-line-region-p ()
  (and (save-excursion (goto-char (region-beginning)) (bolp))
       (save-excursion (goto-char (region-end)) (eolp))))

;;;###autoload
(defun +indent-left (arg)
  (interactive "P")
  (let ((arg (or arg tab-width)))
    (unless (region-active-p)
      (+mark-whole-line))
    (dotimes (_ arg)
      (call-interactively #'indent-rigidly-left))))

;;;###autoload
(defun +indent-right (arg)
  (interactive "P")
  (let ((arg (or arg tab-width)))
    (unless (region-active-p)
      (+mark-whole-line))
    (dotimes (_ arg)
      (call-interactively #'indent-rigidly-right))))

;;;###autoload
(defun beginning-of-indentation-position ()
  (save-excursion
    (back-to-indentation)
    (point)))

;;;###autoload
(defun goto-start-of-region ()
  (if (region-active-p)
      (if (> (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;;;###autoload
(defun goto-end-of-region ()
  (if (region-active-p)
      (if (< (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;; https://stackoverflow.com/questions/2588277/how-can-i-swap-or-replace-multiple-strings-in-code-at-the-same-time
;;;###autoload
(defun parallel-replace (plist &optional start end)
  (interactive
   `(,(cl-loop with input = (read-from-minibuffer "Replace: ")
               with limit = (length input)
               for (item . index) = (read-from-string input 0)
               then (read-from-string input index)
               collect (prin1-to-string item t) until (<= limit index))
     ,@(if (use-region-p) `(,(region-beginning) ,(region-end)))))
  (let* ((alist (cl-loop for (key val . tail) on plist by #'cddr
                         collect (cons key val)))
         (matcher (regexp-opt (mapcar #'car alist) 'words)))
    (save-excursion
      (goto-char (or start (point)))
      (while (re-search-forward matcher (or end (point-max)) t)
        (replace-match (cdr (assoc-string (match-string 0) alist)))))))


;;;###autoload
(defun +rectangle-mode (&optional arg)
  (interactive "p")
  (rectangle-mark-mode)
  (forward-char)
  (next-line (- arg 1)))

;;;###autoload
(defun +insert-newlines-above (arg)
  (interactive "p")
  ;; save excursion didn't work if point is at line-beginning-position
  (let ((col (- (point) (line-beginning-position))))
    (beginning-of-line)
    (dotimes (_ arg)
      (newline))
    (forward-char col)))

;;;###autoload
(defun +insert-newlines-below (arg)
  (interactive "p")
  (save-excursion
    (end-of-line)
    (dotimes (_ arg)
      (newline))))

;;;###autoload
(defun insert-newline-indent ()
  (interactive)
  (save-excursion
    (newline)
    (indent-for-tab-command)))

;;;###autoload
(defun +server-edit ()
  (interactive)
  (save-buffer)
  (server-edit))

;;;###autoload
(defun insert-newline-dwim ()
  (interactive)
  (if (region-active-p)
      (insert-newline-preserving-region)
    (progn
      (newline)
      (indent-for-tab-command))))

;;;###autoload
(defun insert-newline-above-dwim ()
  "split the line above at point"
  (interactive)
  (let ((deactivate-mark nil))
    (if (region-active-p)
        (progn
          (insert-newline-preserving-region)
          (save-excursion
            (next-line)
            (indent-for-tab-command)))
      (progn
        (insert-newline-dwim)
        (drag-stuff-up 1)
        (indent-for-tab-command)))))

;;;###autoload
(defun insert-newline-preserving-region ()
  "Insert a newline at point while preserving the region."
  (let* ((was-active (region-active-p))
         (old-point (point))
         (old-mark (and (region-active-p) (mark t)))
         (deactivate-mark nil)
         (point-at-end (eq old-point (region-end)))
         (region-len (abs (- old-point old-mark))))
    (insert "\n")
    (indent-according-to-mode)
    (when point-at-end
      (goto-char old-point)
      (set-mark (- (point) region-len))
      (activate-mark))))

;;;###autoload
(defun +source-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (looking-back ";" nil)
      (insert ";"))))
