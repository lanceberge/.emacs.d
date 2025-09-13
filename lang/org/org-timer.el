(use-package org-timer
  :ensure nil
  :bind
  (:map +leader-map
        ("ot" . +org-timer-toggle)))

(defvar +org-timer-running nil)
(defvar +org-timer-start-time nil)
(defvar +org-timer-start-date nil)
(defvar +org-timer-log-file "~/org-roam/Time_Log.org")

(defun +format-current-time ()
  (format-time-string "%I:%M:%S %p"))

;;;###autoload
(defun +org-timer-toggle ()
  (interactive)
  ;; if the timer is running
  (if org-timer-start-time
      (let ((elapsed-time (org-timer-value-string)))
        (org-timer-stop)
        (+insert-elapsed-time-to-buffer nil elapsed-time +org-timer-start-time (+format-current-time))
        (message "Elapsed time: %s" elapsed-time)
        (setq +org-timer-start-time nil)
        (setq +org-timer-start-date nil))
    (progn
      (org-timer-start)
      (setq +org-timer-start-time (+format-current-time))
      (setq +org-timer-start-date (format-time-string "%m/%d/%y")))))

(defun +insert-elapsed-time-to-buffer (log-file elapsed-time start-time end-time)
  (save-window-excursion
    (find-file +org-timer-log-file)
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\* " (regexp-quote +org-timer-start-date)) nil t)
        (progn
          (goto-char (point-max))
          (+insert--time-logs elapsed-time start-time end-time))
      (progn
        (goto-char (point-max))
        (insert (format "\n* %s\n\n" +org-timer-start-date))
        (+insert--time-logs elapsed-time start-time end-time)))))

(defun +insert--time-logs (elapsed-time start-time end-time)
  (insert (format "  - start: %s, end: %s, elapsed: %s\n" start-time end-time elapsed-time)))

;; TODO compute the total elapsed time in a function called by insert-elapsed-time-to-buffer
