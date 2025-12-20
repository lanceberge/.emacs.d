;;; -*- lexical-binding: t -*-
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
