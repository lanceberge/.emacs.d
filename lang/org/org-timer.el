(use-package org-timer
  :ensure nil
  :bind
  (:map +leader2-map
        ("ot" . #'+org-timer-toggle)
        ("SPC ot" . #'+org-timer-open-file)))

(defvar +org-timer-running nil)
(defvar +org-timer-start-time nil)
(defvar +org-timer-start-date nil)
(defvar +org-timer-log-file "~/org-roam/Time_Log.org")
(defvar +org-timer--start-proj nil)

;;;###autoload
(defun +org-timer-open-file ()
  (interactive)
  (+open-tab-if-exists "org-roam")
  (find-file +org-timer-log-file))

;;;###autoload
(defun +format-current-time ()
  (format-time-string "%I:%M:%S %p"))

;;;###autoload
(defun +org-timer-toggle ()
  ;; TODO prompt for a category and proj
  (interactive)
  ;; if the timer is running
  (if +org-timer-start-time
      (let ((elapsed-time (org-timer-value-string)))
        (org-timer-stop)
        (+insert-elapsed-time-to-buffer nil elapsed-time +org-timer-start-time (+format-current-time))
        (message "Elapsed time: %s" elapsed-time)
        (setq +org-timer-start-time nil)
        (setq +org-timer-start-date nil)
        (setq +org-timer--start-proj nil))
    (progn
      (org-timer-start)
      (setq +org-timer-start-time (+format-current-time))
      (setq +org-timer-start-date (format-time-string "%m/%d/%y"))
      (setq +org-timer--start-proj (+current-proj-tab-name)))))

;;;###autoload
(defun +insert-elapsed-time-to-buffer (log-file elapsed-time start-time end-time)
  (save-window-excursion
    (find-file +org-timer-log-file)
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
      (progn
        (goto-char (point-max))
        (insert (format "\n* %s\n\n" +org-timer-start-date))
        (+org-timer--insert-time-logs elapsed-time start-time end-time)
        (insert (format "  - Total Elapsed: %s\n" elapsed-time))))
    (save-buffer)))

;;;###autoload
(defun +org-timer--insert-time-logs (elapsed-time start-time end-time &optional omit-newline)
  (let ((newline-character (if omit-newline "" "\n")))
    (insert (format "  - Start: %s, End: %s, Elapsed: %s, Proj: %s%s" start-time end-time elapsed-time +org-timer--start-proj newline-character))))

;;;###autoload
(defun +org-timer--sum-hms-times (first second)
  "Sum two time strings FIRST and SECOND in HH:MM:SS format, return result in same format."
  (org-timer-secs-to-hms
   (+ (org-timer-hms-to-secs first)
      (org-timer-hms-to-secs second))))
