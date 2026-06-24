;;; -*- lexical-binding: t -*-

(defgroup +dot-repeat nil
  "Repeat modal editing changes."
  :group 'editing)

(defcustom +dot-repeat-excluded-commands nil
  "Commands that should not become the repeatable edit."
  :type '(repeat symbol))

(defvar +dot-repeat--last-repeat nil
  "Last repeatable modal edit.")

(defvar +dot-repeat--pending-insert-episode nil
  "Insert episode currently being recorded.")

(defvar +dot-repeat--before-buffer nil
  "Buffer current before the command being observed.")

(defvar +dot-repeat--before-tick nil
  "Modified tick before the command being observed.")

(defvar +dot-repeat--before-state nil
  "Modal state before the command being observed.")

(defvar +dot-repeat--before-prefix nil
  "Prefix arg before the command being observed.")

(defvar +dot-repeat--before-command-history nil
  "Most recent command history entry before the command being observed.")

(defvar +dot-repeat--last-command-history nil
  "Most recent command history entry observed after a top-level command.")

(defvar +dot-repeat--replaying nil
  "Non-nil while `+dot-repeat' is replaying an edit.")

(defvar +dot-repeat--suppress-current-command nil
  "Non-nil means skip recording the current command.")

;;;###autoload
(defun +dot-repeat ()
  "Repeat the last recorded modal edit.
This command is intentionally limited to `+normal-mode'."
  (interactive)
  (unless (bound-and-true-p +normal-mode)
    (user-error "Dot repeat is only available in normal mode"))
  (unless +dot-repeat--last-repeat
    (user-error "No edit to repeat"))
  (when (called-interactively-p 'interactive)
    (setq +dot-repeat--suppress-current-command t))
  (let ((+dot-repeat--replaying t))
    (pcase (plist-get +dot-repeat--last-repeat :type)
      ('normal-edit
       (+dot-repeat--call-command
        (plist-get +dot-repeat--last-repeat :command)
        (plist-get +dot-repeat--last-repeat :prefix)
        (plist-get +dot-repeat--last-repeat :form)))
      ('insert-episode
       (+dot-repeat--call-command
        (plist-get +dot-repeat--last-repeat :entry-command)
        (plist-get +dot-repeat--last-repeat :entry-prefix)
        (plist-get +dot-repeat--last-repeat :entry-form))
       (unless (bound-and-true-p +insert-mode)
         (+insert-mode 1))
       (execute-kbd-macro (plist-get +dot-repeat--last-repeat :body-macro))
       (when (and (bound-and-true-p +insert-mode)
                  (fboundp '+normal-mode))
         (+normal-mode 1)))
      (_
       (user-error "Unknown dot repeat: %S" +dot-repeat--last-repeat)))))

;;;###autoload
(define-minor-mode +dot-repeat-mode
  "Record modal edits so `+dot-repeat' can replay the latest one."
  :global t
  (if +dot-repeat-mode
      (progn
        (setq +dot-repeat--last-command-history (car command-history))
        (add-hook 'pre-command-hook #'+dot-repeat--pre-command)
        (add-hook 'post-command-hook #'+dot-repeat--post-command))
    (remove-hook 'pre-command-hook #'+dot-repeat--pre-command)
    (remove-hook 'post-command-hook #'+dot-repeat--post-command)
    (+dot-repeat--clear-pending-insert-episode)))

;;;###autoload
(defun +dot-repeat--pre-command ()
  (when (and (+dot-repeat--current-command)
             (+dot-repeat--top-level-command-p))
    (setq +dot-repeat--before-buffer (current-buffer)
          +dot-repeat--before-tick (buffer-chars-modified-tick)
          +dot-repeat--before-state (+dot-repeat--modal-state)
          +dot-repeat--before-prefix current-prefix-arg
          +dot-repeat--before-command-history (car command-history))))

;;;###autoload
(defun +dot-repeat--post-command ()
  (when (and +dot-repeat-mode
             (+dot-repeat--current-command))
    (unwind-protect
        (cond
         (+dot-repeat--suppress-current-command
          (setq +dot-repeat--suppress-current-command nil)
          (+dot-repeat--clear-pending-insert-episode))
         ((not +dot-repeat--replaying)
          (let ((after-state (+dot-repeat--modal-state))
                (changed-p (+dot-repeat--current-buffer-changed-p))
                (command (+dot-repeat--current-command)))
            (cond
             ((not (eq +dot-repeat--before-buffer (current-buffer)))
              (+dot-repeat--clear-pending-insert-episode))
             ((and +dot-repeat--pending-insert-episode
                   (eq +dot-repeat--before-state 'insert))
              (+dot-repeat--record-insert-command changed-p after-state))
             ((and (eq +dot-repeat--before-state 'normal)
                   (eq after-state 'insert))
              (+dot-repeat--start-insert-episode command
                                                 +dot-repeat--before-prefix
                                                 (+dot-repeat--command-form)
                                                 changed-p))
             ((and changed-p
                   (eq +dot-repeat--before-state 'normal)
                   (eq after-state 'normal)
                   (+dot-repeat--recordable-command-p command))
              (+dot-repeat--record-normal-edit command
                                               +dot-repeat--before-prefix
                                               (+dot-repeat--command-form)))
             ((not (eq after-state 'insert))
              (+dot-repeat--clear-pending-insert-episode))))))
      (setq +dot-repeat--last-command-history (car command-history)))))

;;;###autoload
(defun +dot-repeat--record-normal-edit (command prefix form)
  (setq +dot-repeat--last-repeat
        (list :type 'normal-edit
              :command command
              :prefix prefix
              :form form)))

;;;###autoload
(defun +dot-repeat--start-insert-episode (entry-command entry-prefix entry-form changed-p)
  (if (+dot-repeat--recordable-command-p entry-command)
      (setq +dot-repeat--pending-insert-episode
            (list :entry-command entry-command
                  :entry-prefix entry-prefix
                  :entry-form entry-form
                  :body-macro []
                  :changed-p changed-p))
    (+dot-repeat--clear-pending-insert-episode)))

;;;###autoload
(defun +dot-repeat--record-insert-command (changed-p after-state)
  (when-let ((keys (+dot-repeat--command-keys)))
    (+dot-repeat--append-insert-keys keys))
  (when changed-p
    (plist-put +dot-repeat--pending-insert-episode :changed-p t))
  (when (eq after-state 'normal)
    (+dot-repeat--finish-insert-episode)))

;;;###autoload
(defun +dot-repeat--finish-insert-episode ()
  (when (plist-get +dot-repeat--pending-insert-episode :changed-p)
    (setq +dot-repeat--last-repeat
          (list :type 'insert-episode
                :entry-command (plist-get +dot-repeat--pending-insert-episode
                                          :entry-command)
                :entry-prefix (plist-get +dot-repeat--pending-insert-episode
                                         :entry-prefix)
                :entry-form (plist-get +dot-repeat--pending-insert-episode
                                       :entry-form)
                :body-macro (plist-get +dot-repeat--pending-insert-episode
                                       :body-macro))))
  (+dot-repeat--clear-pending-insert-episode))

;;;###autoload
(defun +dot-repeat--clear-pending-insert-episode ()
  (setq +dot-repeat--pending-insert-episode nil))

;;;###autoload
(defun +dot-repeat--call-command (command prefix &optional form)
  (if form
      (eval form t)
    (let ((current-prefix-arg prefix))
      (call-interactively command))))

;;;###autoload
(defun +dot-repeat--append-insert-keys (keys)
  (plist-put +dot-repeat--pending-insert-episode
             :body-macro
             (vconcat (plist-get +dot-repeat--pending-insert-episode
                                 :body-macro)
                      keys)))

;;;###autoload
(defun +dot-repeat--recordable-command-p (command)
  (and command
       (not (eq command '+dot-repeat))
       (not (memq command +dot-repeat-excluded-commands))))

;;;###autoload
(defun +dot-repeat--current-buffer-changed-p ()
  (and (eq +dot-repeat--before-buffer (current-buffer))
       (not (eq +dot-repeat--before-tick (buffer-chars-modified-tick)))))

;;;###autoload
(defun +dot-repeat--command-form ()
  (let ((form (car command-history)))
    (unless (or (eq form +dot-repeat--before-command-history)
                (eq form +dot-repeat--last-command-history))
      form)))

;;;###autoload
(defun +dot-repeat--current-command ()
  (or (and (boundp 'real-this-command) real-this-command)
      this-command))

;;;###autoload
(defun +dot-repeat--top-level-command-p ()
  (or (not (boundp 'real-this-command))
      (null real-this-command)
      (eq this-command real-this-command)))

;;;###autoload
(defun +dot-repeat--modal-state ()
  (cond
   ((bound-and-true-p +normal-mode) 'normal)
   ((bound-and-true-p +insert-mode) 'insert)
   ((bound-and-true-p +motion-mode) 'motion)
   ((bound-and-true-p +sexp-mode) 'sexp)
   (t nil)))

;;;###autoload
(defun +dot-repeat--command-keys ()
  (let ((keys (this-command-keys-vector)))
    (unless (or (null keys)
                (= (length keys) 0)
                (+dot-repeat--mouse-event-p keys))
      keys)))

;;;###autoload
(defun +dot-repeat--mouse-event-p (keys)
  (catch 'mouse
    (dotimes (i (length keys))
      (when (mouse-event-p (aref keys i))
        (throw 'mouse t)))
    nil))

(provide '+dot-repeat)
