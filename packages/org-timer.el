;;; org-timer-extensions.el --- Org timer logging helpers -*- lexical-binding: t -*-

(require 'org-timer)

(defgroup +org-timer nil
  "Org timer logging helpers."
  :group 'org)

(defvar +org-timer-running nil)
(defvar +org-timer-start-time nil)
(defvar +org-timer-start-date nil)

(defcustom +org-timer-log-file nil
  "File where custom org timer entries are logged."
  :type 'file
  :group '+org-timer)

(defvar +org-timer--start-proj nil)

;;;###autoload
(defun +org-timer-open-file ()
  (interactive)
  (find-file +org-timer-log-file))

;;;###autoload
(defun +org-timer-format-current-time ()
  (format-time-string "%I:%M:%S %p"))

;;;###autoload
(defun +org-timer-toggle ()
  ;; TODO prompt for a category and proj
  (interactive)
  ;; if the timer is running
  (if +org-timer-start-time
      (let ((elapsed-time (org-timer-value-string)))
        (org-timer-stop)
        (+org-timer-insert-elapsed-time-to-buffer
         nil
         elapsed-time
         +org-timer-start-time
         (+org-timer-format-current-time))
        (message "Elapsed time: %s" elapsed-time)
        (setq +org-timer-start-time nil)
        (setq +org-timer-start-date nil)
        (setq +org-timer--start-proj nil))
    (org-timer-start)
    (setq +org-timer-start-time (+org-timer-format-current-time))
    (setq +org-timer-start-date (format-time-string "%m/%d/%y"))
    (setq +org-timer--start-proj (+project--current-proj-name))))

;;;###autoload
(defun +org-timer-insert-elapsed-time-to-buffer
    (log-file elapsed-time start-time end-time)
  (save-window-excursion
    (find-file (or log-file +org-timer-log-file))
    (goto-char (point-min))
    ;; if the heading is there, insert the current elapsed and update total elapsed
    (if (re-search-forward (concat "^\\* " (regexp-quote +org-timer-start-date)) nil t)
        (progn
          (goto-char (point-max))
          ;; Skip above the total elapsed line at the end
          (previous-line 2)
          (end-of-line)
          (insert "\n")
          (+org-timer--insert-time-logs elapsed-time start-time end-time t)
          (next-line)
          (beginning-of-line)
          (when (re-search-forward "Total Elapsed: " nil t)
            ;; update elapsed time
            (let ((previous-elapsed-time
                   (buffer-substring-no-properties (point) (line-end-position))))
              (delete-region (point) (line-end-position))
              (insert (+org-timer--sum-hms-times previous-elapsed-time elapsed-time)))))
      ;; otherwise, insert the heading, current elapsed, and total elapsed (equal to cur elapsed)
      (goto-char (point-max))
      (insert (format "\n* %s\n\n" +org-timer-start-date))
      (+org-timer--insert-time-logs elapsed-time start-time end-time)
      (insert (format "  - Total Elapsed: %s\n" elapsed-time)))
    (save-buffer)))

;;;###autoload
(defun +org-timer--insert-time-logs
    (elapsed-time start-time end-time &optional omit-newline)
  (let ((newline-character (if omit-newline "" "\n")))
    (insert
     (format "  - Start: %s, End: %s, Elapsed: %s, Proj: %s%s"
             start-time
             end-time
             elapsed-time
             +org-timer--start-proj
             newline-character))))

;;;###autoload
(defun +org-timer--sum-hms-times (first second)
  "Sum two time strings FIRST and SECOND in HH:MM:SS format, return result in same format."
  (org-timer-secs-to-hms
   (+ (org-timer-hms-to-secs first)
      (org-timer-hms-to-secs second))))

(provide '+org-timer)
;;; org-timer-extensions.el ends here
