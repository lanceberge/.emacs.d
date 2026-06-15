;;; consult-theme-rotate.el --- Consult interface for theme rotation -*- lexical-binding: t -*-

(require 'consult)
(require 'seq)
(require 'theme-rotate)

(defvar-local +consult-theme-rotate-style nil
  "Theme style currently displayed by `+consult-theme-rotate'.")

(defvar +consult-theme-rotate-minibuffer-mode-map
  (make-sparse-keymap)
  "Keymap active in `+consult-theme-rotate' minibuffers.")

;;;###autoload
(define-minor-mode +consult-theme-rotate-minibuffer-mode
  "Local mode for `+consult-theme-rotate' minibuffers."
  :init-value nil
  :keymap +consult-theme-rotate-minibuffer-mode-map)

(defvar +consult-theme-rotate--initial-style nil
  "Initial style for the active `+consult-theme-rotate' minibuffer.")

;;;###autoload
(defun +consult-theme-rotate--minibuffer-buffer ()
  "Return the active minibuffer buffer, if any."
  (when-let* ((window (active-minibuffer-window)))
    (window-buffer window)))

;;;###autoload
(defun +consult-theme-rotate--current-style ()
  "Return the current minibuffer-local theme rotation style."
  (or (when-let* ((buffer (+consult-theme-rotate--minibuffer-buffer)))
        (buffer-local-value '+consult-theme-rotate-style buffer))
      +consult-theme-rotate-style
      +consult-theme-rotate--initial-style
      +theme-rotate-current-style
      'light))

;;;###autoload
(defun +consult-theme-rotate--themes ()
  "Return themes for the current minibuffer-local style."
  (if (eq (+consult-theme-rotate--current-style) 'dark)
      +theme-rotate-dark-themes
    +theme-rotate-light-themes))

;;;###autoload
(defun +consult-theme-rotate--available-themes ()
  "Return available themes for the current minibuffer-local style."
  (let ((available (custom-available-themes)))
    (seq-filter (lambda (theme) (memq theme available))
                (+consult-theme-rotate--themes))))

;;;###autoload
(defun +consult-theme-rotate--completion-table (string pred action)
  "Complete theme names matching STRING, PRED, and ACTION."
  (complete-with-action
   action
   (mapcar #'symbol-name (+consult-theme-rotate--available-themes))
   string
   pred))

;;;###autoload
(defun +consult-theme-rotate--refresh ()
  "Refresh the active completion UI."
  (when (fboundp 'completion--flush-all-sorted-completions)
    (completion--flush-all-sorted-completions))
  (run-hooks 'consult--completion-refresh-hook))

;;;###autoload
(defun +consult-theme-rotate--select-first ()
  "Select the first displayed completion candidate."
  (when (boundp 'vertico--index)
    (setq vertico--index 0)))

;;;###autoload
(defun +consult-theme-rotate--preview-current ()
  "Preview the currently selected completion candidate."
  (when (functionp consult--preview-function)
    (funcall consult--preview-function)))

;;;###autoload
(defun +consult-theme-rotate-toggle-style ()
  "Toggle the active `+consult-theme-rotate' minibuffer between styles."
  (interactive)
  (unless (minibufferp)
    (user-error "This command is only available in the minibuffer"))
  (setq-local +consult-theme-rotate-style
              (if (eq (+consult-theme-rotate--current-style) 'dark)
                  'light
                'dark))
  (+consult-theme-rotate--refresh)
  (+consult-theme-rotate--select-first)
  (+consult-theme-rotate--preview-current)
  (minibuffer-message
   "%s themes"
   (capitalize (symbol-name +consult-theme-rotate-style))))

;;;###autoload
(defun +consult-theme-rotate--load-theme (theme style)
  "Load THEME and store STYLE in theme rotation state."
  (when theme
    (+theme-rotate--set-current-theme-state theme style))
  (+theme-rotate-load-theme theme))

;;;###autoload
(defun +consult-theme-rotate (&optional style)
  "Select a theme from the configured theme rotation list.
STYLE chooses the initial candidate list.  Toggling style in the
minibuffer is local to that minibuffer until a theme is selected."
  (interactive)
  (let* ((+consult-theme-rotate--initial-style
          (or style +theme-rotate-current-style 'light))
         (saved-theme (car custom-enabled-themes))
         (minibuffer-setup-hook
          (cons (lambda ()
                  (setq-local +consult-theme-rotate-style
                              +consult-theme-rotate--initial-style)
                  (+consult-theme-rotate-minibuffer-mode 1))
                minibuffer-setup-hook)))
    (consult--read
     #'+consult-theme-rotate--completion-table
     :prompt "Theme: "
     :require-match t
     :category 'theme
     :history 'consult--theme-history
     :lookup (lambda (selected &rest _)
               (setq selected (and selected (intern-soft selected)))
               (or (and selected
                        (car (memq selected
                                   (+consult-theme-rotate--available-themes))))
                   saved-theme))
     :state (lambda (action theme)
              (with-selected-window (or (active-minibuffer-window)
                                        (selected-window))
                (pcase action
                  ('return
                   (+consult-theme-rotate--load-theme
                    (or theme saved-theme)
                    (+consult-theme-rotate--current-style)))
                  ('preview
                   (when theme
                     (+theme-rotate-load-theme theme))))))
     :default (symbol-name (or saved-theme
                               (car (+consult-theme-rotate--available-themes)))))))

(provide 'consult-theme-rotate)
;;; consult-theme-rotate.el ends here
