;;; theme-rotate.el --- Theme rotation helpers -*- lexical-binding: t -*-

(require 'seq)
(require 'subr-x)

(defgroup +theme-rotate nil
  "Theme rotation helpers."
  :group 'faces)

(defcustom +theme-rotate-light-themes '(leuven)
  "Light themes to rotate through."
  :type '(repeat symbol)
  :group '+theme-rotate)

(defcustom +theme-rotate-dark-themes '(tango-dark)
  "Dark themes to rotate through."
  :type '(repeat symbol)
  :group '+theme-rotate)

(defcustom +theme-rotate-current-style 'light
  "Current theme style."
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark))
  :group '+theme-rotate)

(defvar +theme-rotate-dark-theme-index 0
  "Index of the current dark theme in `+theme-rotate-dark-themes'.")

(defvar +theme-rotate-light-theme-index 0
  "Index of the current light theme in `+theme-rotate-light-themes'.")

;;;###autoload
(defun +theme-rotate--nth-theme (index themes)
  "Return theme at INDEX in THEMES, wrapping stale indices."
  (when themes
    (nth (mod (or index 0) (length themes)) themes)))

;;;###autoload
(defun +theme-rotate--set-current-theme-state (theme style)
  "Set current rotation state to THEME and STYLE."
  (setq +theme-rotate-current-style style)
  (when-let* ((dark-index (seq-position +theme-rotate-dark-themes theme)))
    (setq +theme-rotate-dark-theme-index dark-index))
  (when-let* ((light-index (seq-position +theme-rotate-light-themes theme)))
    (setq +theme-rotate-light-theme-index light-index)))

;;;###autoload
(defun +theme-rotate-load-current-theme ()
  "Load the current theme for `+theme-rotate-current-style'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (if (eq +theme-rotate-current-style 'dark)
                   (+theme-rotate--nth-theme
                    +theme-rotate-dark-theme-index
                    +theme-rotate-dark-themes)
                 (+theme-rotate--nth-theme
                  +theme-rotate-light-theme-index
                  +theme-rotate-light-themes))))
    (unless theme
      (error "No %s themes configured" +theme-rotate-current-style))
    (load-theme theme t)))

;;;###autoload
(defun +theme-rotate-toggle-dark-light ()
  "Toggle between the configured dark and light theme lists."
  (interactive)
  (if (eq +theme-rotate-current-style 'dark)
      (progn
        (setq +theme-rotate-current-style 'light)
        (+theme-rotate-load-theme
         (+theme-rotate--nth-theme
          +theme-rotate-light-theme-index +theme-rotate-light-themes)))
    (setq +theme-rotate-current-style 'dark)
    (+theme-rotate-load-theme
     (+theme-rotate--nth-theme
      +theme-rotate-dark-theme-index +theme-rotate-dark-themes))))

;;;###autoload
(defun +theme-rotate--rotate (themes index-var &optional reverse)
  "Rotate through THEMES tracked by INDEX-VAR.
When REVERSE is non-nil, rotate backward."
  (if (null themes)
      (error "No themes provided to rotate")
    (let* ((index (symbol-value index-var))
           (increment (if reverse -1 1))
           (next-index (mod (+ increment index) (length themes)))
           (next-theme (nth next-index themes)))
      (set index-var next-index)
      (+theme-rotate-load-theme next-theme))))

;;;###autoload
(defun +theme-rotate-load-theme (theme)
  "Load THEME after disabling currently enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (message "Loading theme: %s" theme)
  (load-theme theme t))

;;;###autoload
(defun +theme-rotate-rotate (&optional reverse)
  "Rotate to the next theme for the current style.
When REVERSE is non-nil, rotate backward."
  (interactive)
  (if (eq +theme-rotate-current-style 'dark)
      (+theme-rotate-rotate-dark reverse)
    (+theme-rotate-rotate-light reverse)))

;;;###autoload
(defun +theme-rotate-prev ()
  "Rotate to the previous theme for the current style."
  (interactive)
  (+theme-rotate-rotate t))

;;;###autoload
(defun +theme-rotate-rotate-dark (&optional reverse)
  "Rotate through `+theme-rotate-dark-themes'.
When REVERSE is non-nil, rotate backward."
  (interactive)
  (+theme-rotate--rotate
   +theme-rotate-dark-themes '+theme-rotate-dark-theme-index reverse))

;;;###autoload
(defun +theme-rotate-rotate-light (&optional reverse)
  "Rotate through `+theme-rotate-light-themes'.
When REVERSE is non-nil, rotate backward."
  (interactive)
  (+theme-rotate--rotate
   +theme-rotate-light-themes '+theme-rotate-light-theme-index reverse))

(add-hook 'desktop-after-read-hook #'+theme-rotate-load-current-theme)

(provide 'theme-rotate)
;;; theme-rotate.el ends here
