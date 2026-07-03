;;; ediff-conflicts.el --- Ediff session for resolving multi-file merge conflicts -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'smerge-mode)
(require 'subr-x)
(require 'ediff)
(require 'project)

(defgroup +ediff nil
  "Extra Ediff commands."
  :group 'ediff)

(defcustom +ediff-conflicts-files-function #'+ediff-conflicts-files-in-root
  "Function called by `+ediff-conflicts' to return conflicted files.
The function is called with no arguments and should return absolute
file names."
  :type 'function
  :group '+ediff)

(cl-defstruct (+ediff-conflicts--session
               (:constructor +ediff-conflicts--make-session))
  files
  (current-index -1)
  target-index
  window-configuration
  pop-current
  active)

(defvar +ediff-conflicts--session nil
  "Current `+ediff-conflicts' session.")

(defvar-keymap +ediff-conflicts-mode-map
  :doc "Keymap for `+ediff-conflicts-mode'."
  "q" #'+ediff-conflicts-continue
  "Q" #'+ediff-conflicts-quit
  "N" #'+ediff-conflicts-next-file
  "P" #'+ediff-conflicts-previous-file)

(define-minor-mode +ediff-conflicts-mode
  "Minor mode for the current `+ediff-conflicts' session."
  :lighter nil
  :keymap +ediff-conflicts-mode-map)

;;;###autoload
(defun +ediff-conflicts--quit-ediff ()
  "Quit the current Ediff session without confirmation."
  (interactive)
  (with-current-buffer ediff-control-buffer
    (setq this-command 'ediff-quit)
    (ediff-really-quit nil)))

;;;###autoload
(defun +ediff-conflicts ()
  "Walk through conflicted files using `smerge-ediff'.
This expects Git-style conflict markers."
  (interactive)
  (when (and (+ediff-conflicts--active-session)
             (not (yes-or-no-p "An Ediff conflict session is active; restart it? ")))
    (user-error "Abort"))
  (let ((files (funcall +ediff-conflicts-files-function)))
    (unless files
      (user-error "No conflicts found"))
    (setq +ediff-conflicts--session
          (+ediff-conflicts--make-session
           :files files
           :window-configuration (current-window-configuration)
           :active t))
    (+ediff-conflicts--next +ediff-conflicts--session)))

;;;###autoload
(defun +ediff-conflicts-quit ()
  "Quit the current `+ediff-conflicts' session after this Ediff."
  (interactive)
  (when-let* ((session (+ediff-conflicts--current-session)))
    (setf (+ediff-conflicts--session-active session) nil
          (+ediff-conflicts--session-target-index session) nil))
  (+ediff-conflicts--quit-ediff))

;;;###autoload
(defun +ediff-conflicts-continue ()
  "Finish the current Ediff and continue the conflict session."
  (interactive)
  (when-let* ((session (+ediff-conflicts--current-session)))
    (setf (+ediff-conflicts--session-pop-current session) t
          (+ediff-conflicts--session-target-index session) nil))
  (+ediff-conflicts--quit-ediff))

;;;###autoload
(defun +ediff-conflicts-next-file ()
  "Visit the next conflicted file in the current Ediff session."
  (interactive)
  (let ((session (+ediff-conflicts--current-session)))
    (setf (+ediff-conflicts--session-target-index session)
          (+ediff-conflicts--wrapped-next-index session)
          (+ediff-conflicts--session-pop-current session) nil))
  (+ediff-conflicts--quit-ediff))

;;;###autoload
(defun +ediff-conflicts-previous-file ()
  "Visit the previous conflicted file in the current Ediff session."
  (interactive)
  (let ((session (+ediff-conflicts--current-session)))
    (setf (+ediff-conflicts--session-target-index session)
          (mod (1- (+ediff-conflicts--session-current-index session))
               (length (+ediff-conflicts--session-files session)))
          (+ediff-conflicts--session-pop-current session) nil))
  (+ediff-conflicts--quit-ediff))

;;;###autoload
(defun +ediff-conflicts--next (session)
  "Open Ediff for the next conflicted file."
  (+ediff-conflicts--visit-index
   session
   (1+ (+ediff-conflicts--session-current-index session))))

;;;###autoload
(defun +ediff-conflicts-files-in-root ()
  "Return files under the current root containing conflict markers."
  (let* ((root (+ediff-conflicts--root))
         (default-directory root)
         (files (if (executable-find "rg")
                    (or (ignore-errors
                          (process-lines "rg" "--files-with-matches"
                                         "--no-messages"
                                         "--glob" "!.git"
                                         "--glob" "!.jj"
                                         "--" "^<<<<<<<"))
                        nil)
                  (+ediff-conflicts--files-with-marker root))))
    (mapcar (lambda (file) (expand-file-name file root)) files)))

;;;###autoload
(defun +ediff-conflicts--visit-index (session index)
  "Open Ediff for conflicted file at INDEX."
  (if-let* ((file (nth index (+ediff-conflicts--session-files session))))
      (condition-case err
          (let ((buffer (find-file-noselect file)))
            (setf (+ediff-conflicts--session-current-index session) index
                  (+ediff-conflicts--session-target-index session) nil)
            (pop-to-buffer buffer)
            (let ((source-contents (+ediff-conflicts--buffer-contents buffer))
                  (source-modified (buffer-modified-p buffer))
                  result-changed)
              (goto-char (point-min))
              (unless (re-search-forward "^<<<<<<<" nil t)
                (error "No conflict markers in %s" file))
              (beginning-of-line)
              (smerge-mode 1)
              (smerge-ediff)
              (let ((initial-result-contents
                     (+ediff-conflicts--buffer-contents ediff-buffer-C)))
                (add-hook 'ediff-quit-hook
                          (lambda ()
                            (setq result-changed
                                  (+ediff-conflicts--prepare-result
                                   source-contents
                                   initial-result-contents)))
                          -90 t))
              (add-hook 'ediff-quit-hook
                        (lambda ()
                          (+ediff-conflicts--finish-file
                           session buffer file result-changed source-modified))
                        90 t)
              (+ediff-conflicts--enable-session-mode)
              (message "%s" (+ediff-conflicts--progress session))))
        (error
         (message "%s" (error-message-string err))
         (+ediff-conflicts--visit-index session (1+ index))))
    (+ediff-conflicts--finish-session session)))

;;;###autoload
(defun +ediff-conflicts--finish-file
    (session source-buffer file result-changed source-modified)
  "Save SOURCE-BUFFER after Ediff finishes FILE, then continue SESSION."
  (ignore file)
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (if result-changed
          (when (buffer-modified-p)
            (save-buffer))
        (set-buffer-modified-p source-modified))))
  (cond
   ((not (+ediff-conflicts--session-active session))
    (+ediff-conflicts--finish-session session))
   ((+ediff-conflicts--session-target-index session)
    (+ediff-conflicts--visit-index
     session
     (+ediff-conflicts--session-target-index session)))
   ((+ediff-conflicts--session-pop-current session)
    (if-let* ((next-index (+ediff-conflicts--pop-current-file session)))
        (+ediff-conflicts--visit-index session next-index)
      (+ediff-conflicts--finish-session session)))
   (t
    (+ediff-conflicts--next session))))

;;;###autoload
(defun +ediff-conflicts--finish-session (session)
  "Restore windows and report remaining conflicts."
  (when (window-configuration-p
         (+ediff-conflicts--session-window-configuration session))
    (set-window-configuration
     (+ediff-conflicts--session-window-configuration session)))
  (setf (+ediff-conflicts--session-active session) nil)
  (when (eq +ediff-conflicts--session session)
    (setq +ediff-conflicts--session nil))
  (let ((remaining (funcall +ediff-conflicts-files-function)))
    (if remaining
        (message "%d remaining conflicts" (length remaining))
      (message "All conflicts resolved"))))

;;;###autoload
(defun +ediff-select-first-difference ()
  "Select the first Ediff difference at startup."
  (when (and (boundp 'ediff-number-of-differences)
             (> ediff-number-of-differences 0))
    (ediff-jump-to-difference 1)))

;;;###autoload
(defun +ediff-conflicts--enable-session-mode ()
  "Enable `+ediff-conflicts-mode' in the current Ediff buffers."
  (dolist (buffer (list (current-buffer)
                        ediff-buffer-A
                        ediff-buffer-B
                        ediff-buffer-C
                        ediff-ancestor-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (+ediff-conflicts-mode 1)
        (force-mode-line-update)))))

;;;###autoload
(defun +ediff-conflicts--progress (session)
  "Return modeline progress for the current conflict session."
  (format "[%d/%d]"
          (1+ (+ediff-conflicts--session-current-index session))
          (length (+ediff-conflicts--session-files session))))

;;;###autoload
(defun +ediff-conflicts--wrapped-next-index (session)
  "Return the next file index for SESSION, wrapping at the end."
  (mod (1+ (+ediff-conflicts--session-current-index session))
       (length (+ediff-conflicts--session-files session))))

;;;###autoload
(defun +ediff-conflicts--pop-current-file (session)
  "Remove the current file from SESSION and return the next index."
  (let* ((index (+ediff-conflicts--session-current-index session))
         (files (+ediff-conflicts--session-files session))
         (remaining (append (cl-subseq files 0 index)
                            (nthcdr (1+ index) files))))
    (setf (+ediff-conflicts--session-files session) remaining
          (+ediff-conflicts--session-current-index session) (1- index)
          (+ediff-conflicts--session-target-index session) nil
          (+ediff-conflicts--session-pop-current session) nil)
    (when remaining
      (mod index (length remaining)))))

;;;###autoload
(defun +ediff-conflicts--current-session ()
  "Return the current conflict session or signal an error."
  (or +ediff-conflicts--session
      (user-error "No active Ediff conflict session")))

;;;###autoload
(defun +ediff-conflicts--active-session ()
  "Return the current active conflict session, or nil."
  (and +ediff-conflicts--session
       (+ediff-conflicts--session-active +ediff-conflicts--session)
       +ediff-conflicts--session))

;;;###autoload
(defun +ediff-conflicts--root ()
  "Return the current project or VC root."
  (or (vc-root-dir)
      (when-let* ((project (project-current)))
        (project-root project))
      default-directory))

;;;###autoload
(defun +ediff-conflicts--files-with-marker (root)
  "Return files under ROOT containing conflict markers without using rg."
  (let (files)
    (dolist (file (directory-files-recursively root ".*" nil t))
      (when (and (file-regular-p file)
                 (not (string-match-p "/\\.\\(?:git\\|jj\\)/" file))
                 (+ediff-conflicts--file-has-marker-p file))
        (push (file-relative-name file root) files)))
    (nreverse files)))

;;;###autoload
(defun +ediff-conflicts--file-has-marker-p (file)
  "Return non-nil if FILE contains a conflict start marker."
  (with-temp-buffer
    (insert-file-contents file nil 0 100000)
    (goto-char (point-min))
    (re-search-forward "^<<<<<<<" nil t)))

;;;###autoload
(defun +ediff-conflicts--prepare-result (source-contents initial-result-contents)
  "Return non-nil when Ediff result differs from INITIAL-RESULT-CONTENTS.
When unchanged, restore SOURCE-CONTENTS into the Ediff result buffer
before `smerge-ediff' writes it back to the source buffer."
  (let ((changed (not (string= initial-result-contents
                               (+ediff-conflicts--buffer-contents
                                ediff-buffer-C)))))
    (unless changed
      (with-current-buffer ediff-buffer-C
        (erase-buffer)
        (insert source-contents)
        (set-buffer-modified-p nil)))
    changed))

;;;###autoload
(defun +ediff-conflicts--buffer-contents (buffer)
  "Return BUFFER contents as a string."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'ediff-conflicts)
;;; ediff-conflicts.el ends here
