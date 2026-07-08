;;; -*- lexical-binding: t -*-
;;;###autoload
(defun read-file-contents (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

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
      (+mark-whole-lines 1))
    (dotimes (_ arg)
      (call-interactively #'indent-rigidly-left))))

;;;###autoload
(defun +indent-right (arg)
  (interactive "P")
  (let ((arg (or arg tab-width)))
    (unless (region-active-p)
      (+mark-whole-lines 1))
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

(defmacro with-point-at-region-end (&rest body)
  `(let ((orig-point (point))
         (orig-mark (mark))
         (orig-point-at-beginning (eq (point) (region-beginning))))
     (unwind-protect
         (progn
           (when (and (region-active-p)
                      orig-point-at-beginning)
             (exchange-point-and-mark))
           ,@body)
       (when (and (region-active-p)
                  orig-point-at-beginning
                  (eq (point) (region-end)))
         (exchange-point-and-mark)))))

(defmacro with-point-at-region-beginning (&rest body)
  `(let ((orig-point (point))
         (orig-mark (mark))
         (orig-point-at-end (eq (point) (region-end))))
     (unwind-protect
         (progn
           (when (and (region-active-p)
                      orig-point-at-end)
             (exchange-point-and-mark))
           ,@body)
       (when (and (region-active-p)
                  orig-point-at-end
                  (eq (point) (region-beginning)))
         (exchange-point-and-mark)))))

;;;###autoload
(defun +move-point-to-region-end ()
  (if (and (region-active-p)
           (eq (point) (region-beginning)))
      (exchange-point-and-mark)))

;;;###autoload
(defun +move-point-to-region-beginning ()
  (if (and (region-active-p)
           (eq (point) (region-end)))
      (exchange-point-and-mark)))

;;;###autoload
(defun +project--current-proj-name ()
  (require 'project)
  (project-root (project-current nil)))

(defun +project--buffer-relative-path ()
  "Return the buffer's path relative to the project-root"
  (require 'project)
  (when-let ((project (project-current t))
             (root (project-root project))
             (file (buffer-file-name)))
    (file-relative-name file root)))

(provide 'editor-lisp)
