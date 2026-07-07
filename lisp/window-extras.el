;;; window-extras.el --- Window helper commands -*- lexical-binding: t -*-

(require 'seq)
(require 'window)
(require 'project)
(require 'ace-window)
(require 'switchy-window)
(require 'project-extras)
(require 'pulsar)

(defvar-local +window--preserved-minibuffer-window-state nil
  "Window state to restore after the current minibuffer exits.")

(defvar +window--pulsar-last-buffer nil
  "Buffer in the selected window the last time Pulsar pulsed.")

;;;###autoload
(defun +window-new-prefix ()
  "Display the buffer of the next command in a newly-created window."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_buffer _alist)
     (cons (+window-split-new) 'window))
   nil "[new-window]")
  (message "Display next command buffer in a new window..."))

;;;###autoload
(defun +window-other-previous ()
  "Move to the previous window, splitting the frame first when needed."
  (interactive)
  (if (one-window-p)
      (split-window-sensibly)
    (other-window -1)))

;;;###autoload
(defun +window-ace-find-file (file)
  "Open FILE in an ace-selected window."
  (interactive
   (let ((from-minibuffer (active-minibuffer-window)))
     (when from-minibuffer
       (+window-preserve-minibuffer-windows))
     (let ((read-minibuffer-restore-windows
            (if from-minibuffer nil read-minibuffer-restore-windows)))
       (list (read-file-name "Find file: ")))))
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+window--ensure-ace-dispatch-window)
    (select-window (aw-select " Ace - Find File"))
    (find-file file)
    (+window-preserve-minibuffer-windows)))

;;;###autoload
(defun +window-ace-switch-to-buffer (buffer)
  "Open BUFFER in an ace-selected window."
  (interactive "BSwitch to buffer: ")
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+window--ensure-ace-dispatch-window)
    (select-window (aw-select " Ace - Switch Buffer"))
    (switch-to-buffer buffer)))

;;;###autoload
(defun +window-find-file-new (file)
  "Open FILE in a newly-created window."
  (interactive
   (let ((from-minibuffer (active-minibuffer-window)))
     (when from-minibuffer
       (+window-preserve-minibuffer-windows))
     (let ((read-minibuffer-restore-windows
            (if from-minibuffer nil read-minibuffer-restore-windows)))
       (list (read-file-name "Find file: ")))))
  (set-window-buffer (+window-split-new) (find-file-noselect file))
  (+window-preserve-minibuffer-windows))

;;;###autoload
(defun +window-find-file-new-largest (file)
  "Open FILE in a new window split from the largest visible window."
  (interactive "FFind file: ")
  (set-window-buffer
   (+window-split-new (+window-largest-visible-window))
   (find-file-noselect file))
  (+window-preserve-minibuffer-windows))

;;;###autoload
(defun +window-find-file-new-largest-action (file)
  "Open Embark file target FILE in a new split from the largest window."
  (+window-find-file-new-largest file))

;;;###autoload
(defun +window-switch-to-buffer-new (buffer)
  "Open BUFFER in a newly-created window."
  (interactive "BSwitch to buffer: ")
  (set-window-buffer (+window-split-new) (get-buffer-create buffer))
  (+window-preserve-minibuffer-windows))

;;;###autoload
(defun +window-switch-to-buffer-new-action (buffer)
  "Open Embark buffer target BUFFER in a split from the largest window."
  (set-window-buffer
   (+window-split-new (+window-largest-visible-window))
   (get-buffer-create buffer))
  (+window-preserve-minibuffer-windows))

;;;###autoload
(defun +window-preserve-minibuffer-windows ()
  "Keep window changes made while reading from the active minibuffer."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (setq-local read-minibuffer-restore-windows nil)
      (setq-local +window--preserved-minibuffer-window-state
                  (cons (window-frame window)
                        (window-state-get
                         (frame-root-window (window-frame window))
                         t)))
      (add-hook 'minibuffer-exit-hook
                #'+window--restore-preserved-minibuffer-windows nil t))))

;;;###autoload
(defun +window-largest-visible-window ()
  "Return the largest visible non-minibuffer window on the selected frame."
  (get-largest-window nil nil nil t))

;;;###autoload
(defun +window-split-new (&optional window)
  "Split WINDOW and return the new window.
WINDOW defaults to the active minibuffer's originating window, or
the selected window when no minibuffer is active."
  (or (seq-some #'+window-split-window
                (delq nil (delete-dups
                           (append
                            (list window
                                  (minibuffer-selected-window)
                                  (get-mru-window nil nil nil t)
                                  (get-largest-window nil nil nil t)
                                  (selected-window))
                            (window-list nil 'no-minibuf)))))
      (user-error "Cannot split window")))

;;;###autoload
(defun +window-split-window (window)
  "Split WINDOW and return the new window, or nil if it cannot be split."
  (unless (window-minibuffer-p window)
    (or (split-window-sensibly window)
        (let* ((width (window-total-width window))
               (height (window-total-height window))
               (prefer-side-by-side (> width (* height 2)))
               (preferred (if prefer-side-by-side
                              #'split-window-right
                            #'split-window-below))
               (fallback (if prefer-side-by-side
                             #'split-window-below
                           #'split-window-right)))
          (or (funcall preferred nil window)
              (funcall fallback nil window))))))

;;;###autoload
(defun +window-split-if-single (&rest _)
  "Split the frame if there is a single window."
  (when (one-window-p)
    (split-window-sensibly)))

;;;###autoload
(defun +window-switchy-other-window ()
  "Return the window `switchy-window' would switch to after its delay."
  (require 'switchy-window)
  (let ((switchy-window--tick-counter switchy-window--tick-counter)
        (switchy-window--tick-alist (copy-tree switchy-window--tick-alist))
        (switchy-window--visited-windows switchy-window--visited-windows)
        (last-command nil)
        target)
    (let ((window-selection-change-functions
           (remove #'switchy-window--on-window-selection-change
                   window-selection-change-functions)))
      (save-window-excursion
        (switchy-window)
        (setq target (selected-window))))
    target))

;;;###autoload
;; TODO this should be improved. it should save the window config for the other buffers when you call it.
;; this enables you to swap buffers without undoing them - only undo the other unzoomed tabs
(defun +window-toggle-tab-zoom ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and tab-bar-mode
           (equal (selected-window) (next-window)))
      (tab-bar-history-back)
    ;; Force tab-bar history to record this command even if it was repeated.
    (setq tab-bar-history-done-command nil)
    (delete-other-windows)))

;;;###autoload
(defun +window-revert-buffer ()
  "Revert the current buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun +other-buffer ()
  "Switch to the most recently selected other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun +window-pulsar-pulse-line-no-minibuffer (frame-or-window)
  "Pulse the selected line unless FRAME-OR-WINDOW shows a minibuffer."
  (let* ((win (if (framep frame-or-window)
                  (frame-selected-window frame-or-window)
                frame-or-window))
         (buf (window-buffer win)))
    (unless (or (minibufferp buf)
                (eq buf +window--pulsar-last-buffer))
      (setq +window--pulsar-last-buffer buf)
      (with-selected-window win
        (pulsar-pulse-line)))))

;;;###autoload
(defun +window--restore-preserved-minibuffer-windows ()
  "Restore `+window--preserved-minibuffer-window-state' after minibuffer exit."
  (when-let ((state +window--preserved-minibuffer-window-state))
    (run-at-time 0 nil #'+window--restore-frame-window-state
                 (car state)
                 (cdr state))))

;;;###autoload
(defun +window--restore-frame-window-state (frame state)
  "Restore FRAME's root window STATE when FRAME is still live."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (window-state-put state (frame-root-window frame) 'safe))))

;;;###autoload
(defun +window--ensure-ace-dispatch-window ()
  "Create a second window when ace-window would have no target choice."
  (when (= (length (window-list nil 'no-minibuf)) 1)
    (+window-split-new)))

(provide 'window-extras)
