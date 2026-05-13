;;; -*- lexical-binding: t -*-
(require 'seq)
(require 'subr-x)

(use-package kanagawa-themes)

(use-package +theme-rotate
  :ensure nil
  :bind
  (:repeat-map theme-rotate-repeat-map
               ("n" . #'+themes-rotate)
               ("l" . #'+themes-toggle-dark-light)
               ("p" . #'+themes-prev))
  (:map +leader2-map
        ("tn" . #'+themes-rotate)
        ("tj" . #'+themes-toggle-dark-light)
        ("tp" . #'+themes-prev)))

;; tsdh-light, leuven, whiteboard, doom-one-light, doom-tomorrow-day, dichromacy, doom-acario-light, doom-flatwhite, doom-earl-gray, doom-feather-light
;; doom-tokyo-night, doom-rouge, doom-monokai-spectrum, monokai-ristretto doom-city-lights, doom-horizon, doom-one, doom-ayu-mirage

(use-package doom-themes)

(setq +themes-dark-themes '(gruvbox-dark-hard kanagawa-wave doom-badger doom-oceanic-next doom-tomorrow-night
                                              doom-spacegrey doom-tokyo-night
                                              doom-palenight doom-Iosvkem doom-one doom-dark+ doom-monokai-octagon ))

(setq +themes-light-themes '(tango kanagawa-lotus doom-nord-light doom-oksolar-light doom-solarized-light tsdh-light doom-opera-light))

(defvar +themes-dark-theme-index 0
  "Index of the current dark theme in `+themes-dark-themes'.")

(defvar +themes-light-theme-index 0
  "Index of the current light theme in `+themes-light-themes'.")

(defvar +themes-current-style 'dark)

(defvar +themes-omarchy-theme-map
  "Map Omarchy theme names to Emacs themes and styles.")

(setq +themes-omarchy-theme-map
      '(("aether" . (doom-miramare . dark))
        ("catppuccin" . (doom-palenight . dark))
        ("catppuccin-latte" . (doom-one-light . light))
        ("ethereal" . (doom-ephemeral . dark))
        ("everforest" . (doom-miramare . dark))
        ("flexoki-light" . (doom-one-light . light))
        ("gruvbox" . (gruvbox-dark-hard . dark))
        ("hackerman" . (doom-1337 . dark))
        ("kanagawa" . (kanagawa-wave . dark))
        ("lumon" . (doom-nova . dark))
        ("matte-black" . (doom-badger . dark))
        ("miasma" . (doom-miramare . dark))
        ("nord" . (doom-nord . dark))
        ("osaka-jade" . (doom-miramare . dark))
        ("retro-82" . (doom-oceanic-next . dark))
        ("ristretto" . (doom-monokai-ristretto . dark))
        ("rose-pine" . (doom-nord-light . light))
        ("tokyo-night" . (doom-tokyo-night . dark))
        ("vantablack" . (doom-plain-dark . dark))
        ("white" . (doom-one-light . light))))

(defun +themes--set-current-theme-state (theme style)
  (setq +themes-current-style style)
  (when-let* ((dark-index (seq-position +themes-dark-themes theme)))
    (setq +themes-dark-theme-index dark-index))
  (when-let* ((light-index (seq-position +themes-light-themes theme)))
    (setq +themes-light-theme-index light-index)))

(defun +themes--omarchy-current-style ()
  (let ((colors-file (expand-file-name "~/.config/omarchy/current/theme/colors.toml")))
    (if (and (file-readable-p colors-file)
             (with-temp-buffer
               (insert-file-contents colors-file)
               (goto-char (point-min))
               (when (re-search-forward "^background = \"#\\([[:xdigit:]]\\{6\\}\\)\"" nil t)
                 (let* ((hex (match-string 1))
                        (red (string-to-number (substring hex 0 2) 16))
                        (green (string-to-number (substring hex 2 4) 16))
                        (blue (string-to-number (substring hex 4 6) 16)))
                   (> (+ (* 0.2126 red) (* 0.7152 green) (* 0.0722 blue)) 128)))))
        'light
      'dark)))

(defun +themes--omarchy-fallback-theme ()
  (if (eq (+themes--omarchy-current-style) 'light)
      '(doom-one-light . light)
    '(gruvbox-dark-hard . dark)))

(add-hook 'desktop-after-read-hook '+themes-load-current-theme)

;;;###autoload
(defun +themes-load-current-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if (eq +themes-current-style 'dark)
      (load-theme (nth +themes-dark-theme-index +themes-dark-themes) t)
    (load-theme (nth +themes-light-theme-index +themes-light-themes) t)))

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
(defun +themes-load-omarchy-theme (theme-name)
  (interactive "sOmarchy theme: ")
  (let* ((theme-key (string-trim theme-name))
         (theme-style (or (cdr (assoc-string theme-key +themes-omarchy-theme-map t))
                          (+themes--omarchy-fallback-theme)))
         (theme (car theme-style))
         (style (cdr theme-style)))
    (+themes--set-current-theme-state theme style)
    (+themes-load-theme theme)))

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
