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
  (project-root (project-current nil)))
