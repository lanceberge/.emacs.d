;;; tabspace-extras.el --- Tabspace-aware navigation helpers -*- lexical-binding: t -*-

(require 'project)
(require 'project-extras)
(require 'consult)

;;;###autoload
(defun +tabspace-other-buffer-dwim (n &optional project)
  "Switch to the N'th most recent file-visiting buffer in PROJECT's tabspace.
Fall back to `+project-other-buffer' when no matching local buffer exists."
  (interactive "p")
  (if-let ((buffer (+tabspace-other-buffer n project)))
      (switch-to-buffer buffer)
    (+project-other-buffer n project)))

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
  "Kill the current buffer and switch to the most recent project tabspace buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (project (project-current nil))
         (project-root-dir (when project
                             (expand-file-name (project-root project)))))
    (if-let ((other-buffer (and project-root-dir
                                (+tabspace-other-buffer 1 project-root-dir))))
        (progn
          (switch-to-buffer other-buffer)
          (kill-buffer buffer))
      (+project-kill-buffer))))

;;;###autoload
(defun +tabspace-other-buffer (n &optional project)
  "Return the N'th most recent file-visiting buffer in PROJECT's tabspace."
  (let ((project (+tabspace-project project)))
    (nth (1- (abs n))
         (seq-remove
          (lambda (buffer)
            (or (eq buffer (current-buffer))
                (not (buffer-file-name buffer))
                (not (+tabspace-buffer-in-project-p buffer project))))
          (+tabspace-local-buffer-list)))))

;;;###autoload
(defun +tabspace-other-special-buffer-buffer ()
  "Return the most recent special buffer in the current project's tabspace."
  (let ((project (project-current t)))
    (seq-find
     (lambda (buffer)
       (and (not (eq buffer (current-buffer)))
            (not (buffer-file-name buffer))
            (not (string-prefix-p " *Minibuf-" (buffer-name buffer)))
            (+tabspace-buffer-in-project-p buffer project)))
     (+tabspace-local-buffer-list))))

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
