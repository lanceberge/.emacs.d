(use-package +themes
  :ensure nil
  :bind
  (:repeat-map theme-rotate-repeat-map
               ("n" . #'+themes-rotate)
               ("l" . #'+themes-toggle-dark-light)
               ("p" . #'+themes-prev))
  (:map +leader2-map
        ("tn" . #'+themes-rotate)
        ("tl" . #'+themes-toggle-dark-light)
        ("tp" . #'+themes-prev)))

(setq +themes-dark-themes '(doom-badger gruvbox nordic-night doom-nord-aurora))
(setq +themes-light-themes '(doom-nord-light tango doom-oksolar-light modus-operandi-tinted modus-operandi-tritanopia doom-earl-grey))

(defvar +themes-dark-theme-index 0
  "Index of the current dark theme in `+themes-dark-themes'.")

(defvar +themes-light-theme-index 0
  "Index of the current light theme in `+themes-light-themes'.")

(defvar +themes-current-style 'dark)

;;;###autoload
(defun +themes-toggle-dark-light ()
  (interactive)
  (if (eq +themes-current-style 'dark)
      (progn
        (setq +themes-current-style 'light)
        (+themes-load-theme (nth +themes-light-theme-index +themes-light-themes)))
    (progn
      (setq +themes-current-style 'dark)
      (+themes-load-theme (nth +themes-dark-theme-index +themes-dark-themes)))))

;;;###autoload
(defun +themes--rotate (themes index-var &optional reverse)
  (if (null themes)
      (error "No themes provided to rotate")
    (let* ((index (symbol-value index-var))
           (increment (if reverse -1 1))
           (next-index (mod (+ increment index) (length themes)))
           (next-theme (nth next-index themes)))
      (set index-var next-index)
      (+themes-load-theme next-theme))))

;;;###autoload
(defun +themes-load-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (message "Loading theme: %s" theme)
  (load-theme theme t))

;;;###autoload
(defun +themes-rotate (&optional reverse)
  (interactive)
  (if (eq +themes-current-style 'dark)
      (+themes-rotate-dark reverse)
    (+themes-rotate-light reverse)))

;;;###autoload
(defun +themes-prev ()
  (interactive)
  (+themes-rotate t))

;;;###autoload
(defun +themes-rotate-dark (&optional reverse)
  (interactive)
  (+themes--rotate +themes-dark-themes '+themes-dark-theme-index reverse))

;;;###autoload
(defun +themes-rotate-light (&optional reverse)
  (interactive)
  (+themes--rotate +themes-light-themes '+themes-light-theme-index reverse))
