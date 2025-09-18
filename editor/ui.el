;;; -*- lexical-binding: t -*-
(use-package frameset
  :ensure nil
  :demand t
  :config
  (setq frameset-filter-alist
        (append '((background-color . :never)
                  (foreground-color . :never)
                  (background-mode . :never)
                  (cursor-color . :never)
                  (mouse-color . :never)
                  (border-color . :never)
                  (scroll-bar-foreground . :never)
                  (scroll-bar-background . :never))
                frameset-filter-alist)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package paren ; show matching parentheses
  :ensure nil
  :hook ((prog-mode text-mode) . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package hydra)

(use-package fringe
  :ensure nil)

(use-package +themes
  :bind
  (:map +leader2-map
        ("tn" . #'+themes-rotate)
        ("tl" . #'+themes-toggle-dark-light)
        ("tp" . #'+themes-prev))
  :ensure nil)

(use-package ef-themes)
(use-package nordic-night-theme)

(setq +themes-dark-themes '(doom-badger gruvbox nordic-night doom-nord-aurora))
(setq +themes-light-themes '(tango doom-nord-light doom-earl-grey doom-oksolar-light))

;; (setq +themes-light-themes '(modus-operandi ef-maris-light ef-owl ef-maris-light ef-spring ef-light ef-frost ef-day))
;; (setq +themes-dark-themes '(doom-monokai-machine modus-vivendi ef-elea-dark))

(defvar +themes-dark-theme-index 0
  "Index of the current dark theme in `+themes-dark-themes'.")
(defvar +themes-light-theme-index 0
  "Index of the current light theme in `+themes-light-themes'.")

(defvar +themes-current-style 'dark)

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

(defun +themes-prev ()
  (interactive)
  (+themes-rotate t))

(defun +themes-rotate-dark (&optional reverse)
  (interactive)
  (+themes--rotate +themes-dark-themes '+themes-dark-theme-index reverse))

(defun +themes-rotate-light (&optional reverse)
  (interactive)
  (+themes--rotate +themes-light-themes '+themes-light-theme-index reverse))
