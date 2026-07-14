;;; -*- lexical-binding: t -*-
(require 'vertico)

(defvar +vertico-window-exit-prefix nil
  "Window display prefix to apply on the next `vertico-exit'.")
(make-variable-buffer-local '+vertico-window-exit-prefix)

;;;###autoload
(defun +vertico-exit (&optional arg)
  "Exit Vertico, applying the pending window prefix first."
  (interactive "P")
  (let ((this-command #'vertico-exit))
    (+vertico-apply-window-exit-prefix)
    (vertico-exit arg)))

;;;###autoload
(defun +vertico-toggle-other-window-exit ()
  "Toggle `other-window-prefix' for the next `vertico-exit'."
  (interactive)
  (+vertico-toggle-window-exit-prefix 'other-window))

;;;###autoload
(defun +vertico-toggle-new-window-exit ()
  "Toggle `+window-new-prefix' for the next `vertico-exit'."
  (interactive)
  (+vertico-toggle-window-exit-prefix 'new-window))

;;;###autoload
(defun +vertico-toggle-window-exit-prefix (prefix)
  "Toggle PREFIX as the window action for the next `vertico-exit'."
  (setq +vertico-window-exit-prefix
        (unless (eq +vertico-window-exit-prefix prefix)
          prefix))
  (message "Vertico exit window: %s"
           (pcase +vertico-window-exit-prefix
             ('other-window "other")
             ('new-window "new")
             (_ "default"))))

;;;###autoload
(defun +vertico-apply-window-exit-prefix ()
  "Apply the pending Vertico window exit prefix."
  (pcase +vertico-window-exit-prefix
    ('other-window (other-window-prefix))
    ('new-window (+window-new-prefix)))
  (setq +vertico-window-exit-prefix nil))

;;;###autoload
(defun +vertico-clear-window-exit-prefix ()
  "Clear the pending Vertico window exit prefix."
  (setq +vertico-window-exit-prefix nil))

;;;###autoload
(defun +auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'+auto-create-missing-dirs)

(provide 'vertico-extras)
