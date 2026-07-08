;;; tabspace-extras.el --- Tabspace-aware navigation helpers -*- lexical-binding: t -*-

(require 'project)
(require 'project-extras)
(require 'project-tab)
(require 'cl-lib)
(require 'consult)
(require 'tabspaces)

(defgroup +tabspace nil
  "Tabspace-aware navigation helpers."
  :group 'convenience)

(defcustom +tabspace-switch-buffer-predicate
  #'+tabspace-switch-buffer-default-predicate
  "Predicate for `+tabspace-other-bufffer' commands."
  :type 'function
  :group '+tabspace)

(defcustom +tabspace-switch-special-buffer-predicate
  #'+tabspace-switch-special-buffer-default-predicate
  "Predicate for `+tabspace-other-special-buffer' commands."
  :type 'function
  :group '+tabspace)

;;;###autoload
(defun +tabspace-other-project-buffer-dwim (n &optional project)
  "Switch to the N'th most recent file-visiting buffer in PROJECT's tabspace.
Fall back to `+project-other-buffer' when no matching local buffer exists."
  (interactive "p")
  (if-let ((buffer (+tabspace-other-buffer-in-project n project)))
      (switch-to-buffer buffer)
    (+project-other-buffer n project)))

;;;###autoload
(defun +tabspace-other-buffer-dwim (n)
  (interactive "p")
  (if-let ((buffer (+tabspace-other-buffer n)))
      (switch-to-buffer buffer)
    (+project-other-buffer n (project-root (project-current nil)))))

;;;###autoload
(defun +tabspace-other-special-buffer-dwim (arg)
  "Switch between real and special buffers in the current project tabspace.
Fall back to `+project-other-special-buffer-dwim' when no matching local buffer
exists."
  (interactive "p")
  (if (buffer-file-name (current-buffer))
      (+tabspace-other-special-buffer)
    (+tabspace-other-buffer-dwim arg)))

;;;###autoload
(defun +tabspace-other-special-buffer ()
  "Switch to the most recent special buffer in the current project tabspace."
  (interactive)
  (if-let ((buffer (+tabspace-other-special-buffer-buffer)))
      (switch-to-buffer buffer)
    (call-interactively #'+project-other-special-buffer)))

;;;###autoload
(defun +tabspace-kill-buffer-dwim ()
  "Kill or remove the current buffer, then switch to a recent project buffer.
Kill the buffer when it only belongs to the current tabspace.  Otherwise remove
it from the current tabspace."
  (interactive)
  (let* ((buffer (current-buffer)))
    (if-let ((other-buffer (+tabspace-other-buffer 1)))
        (progn
          (switch-to-buffer other-buffer)
          (+tabspace-kill-or-remove-buffer buffer))
      (switch-to-buffer (+project-other-buffer-buffer 1))
      (+tabspace-kill-or-remove-buffer buffer))))

;;;###autoload
(defun +tabspace-other-buffer-in-project (n &optional project)
  "Return the N'th most recent file-visiting buffer in PROJECT's tabspace."
  (let ((project (+tabspace-project project))
        (index (max 0 (1- (abs n)))))
    (cl-loop for buffer in (+tabspace-local-buffer-list)
             unless (or (eq buffer (current-buffer))
                        (not (funcall +tabspace-switch-buffer-predicate buffer))
                        (not (+tabspace-buffer-in-project-p buffer project)))
             if (zerop index)
             return buffer
             else do (cl-decf index))))

;;;###autoload
(defun +tabspace-other-buffer (n)
  "Return the N'th most recent file-visiting buffer in PROJECT's tabspace."
  (let ((index (max 0 (1- (abs n)))))
    (cl-loop for buffer in (+tabspace-local-buffer-list)
             unless (or (eq buffer (current-buffer))
                        (not (funcall +tabspace-switch-buffer-predicate buffer)))
             if (zerop index)
             return buffer
             else do (cl-decf index))))

;;;###autoload
(defun +tabspace-kill-or-remove-buffer (buffer)
  "Kill BUFFER if it belongs only to this tabspace, otherwise remove it.
When called interactively, use the current buffer."
  (interactive (list (current-buffer)))
  (if (+tabspace-buffer-in-other-tabspace-p buffer)
      (tabspaces-remove-buffer buffer)
    (kill-buffer buffer)
    (message "Killed buffer")))

;;;###autoload
(defun +tabspace-buffer-in-other-tabspace-p (buffer)
  "Return non-nil when BUFFER belongs to another tabspace."
  (or (member (buffer-name buffer) tabspaces-include-buffers)
      (let* ((tabs (funcall tab-bar-tabs-function))
             (current-index (tab-bar--current-tab-index tabs))
             (prefix (+project-tab--current-prefix)))
        (cl-loop for tab in tabs
                 for index from 0
                 unless (= index current-index)
                 thereis
                 (and (or (not prefix)
                          (+project-tab--tab-p tab prefix))
                      (cl-loop for tab-buffer in (tabspaces--buffer-list nil index)
                               thereis (eq tab-buffer buffer)))))))

;;;###autoload
(defun +tabspace-other-special-buffer-buffer ()
  "Return the most recent special buffer in the current project's tabspace."
  (seq-find
   (lambda (buffer)
     (and (not (eq buffer (current-buffer)))
          (funcall +tabspace-switch-special-buffer-predicate buffer)))
   (+tabspace-local-buffer-list)))

;;;###autoload
(defun +tabspace-switch-buffer-default-predicate (buffer)
  "Return non-nil when BUFFER should be used by regular tabspace commands."
  (with-current-buffer buffer
    (or buffer-file-name
        (derived-mode-p 'eshell-mode 'eat-mode 'gptel-mode 'agent-shell-mode))))

;;;###autoload
(defun +tabspace-switch-special-buffer-default-predicate (buffer)
  "Return non-nil when BUFFER should be used by special tabspace commands."
  (with-current-buffer buffer
    (and (not buffer-file-name)
         (not (string-prefix-p " " (buffer-name buffer))))))

;;;###autoload
(defun +tabspace-local-buffer-list ()
  "Return the selected frame's tabspace-local live buffers."
  (seq-filter #'buffer-live-p (frame-parameter nil 'buffer-list)))

;;;###autoload
(defun +tabspace-project (&optional project)
  "Return PROJECT as a project object, defaulting to the current project."
  (cond
   ((stringp project) (project-current t project))
   (project)
   ((project-current nil))))

;;;###autoload
(defun +tabspace-buffer-in-project-p (buffer project)
  "Return non-nil when BUFFER belongs to PROJECT."
  (when project
    (when-let ((dir (buffer-local-value 'default-directory buffer)))
      (string-prefix-p
       (expand-file-name (file-name-as-directory (project-root project)))
       (expand-file-name dir)))))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  ;; (plist-put consult-source-buffer :hidden t)
  ;; (plist-put consult-source-buffer :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(provide 'tabspace-extras)
