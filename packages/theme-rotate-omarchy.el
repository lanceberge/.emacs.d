;;; -*- lexical-binding: t -*-
(defvar +theme-rotate-omarchy-theme-map
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
    ("matte-black" . (kanagawa-wave . dark))
    ("miasma" . (doom-miramare . dark))
    ("nord" . (doom-nord . dark))
    ("osaka-jade" . (doom-miramare . dark))
    ("retro-82" . (doom-oceanic-next . dark))
    ("ristretto" . (doom-monokai-ristretto . dark))
    ("rose-pine" . (doom-nord-light . light))
    ("tokyo-night" . (doom-tokyo-night . dark))
    ("vantablack" . (doom-plain-dark . dark))
    ("white" . (doom-one-light . light))
    ("ochre-light" . (kanagawa-paper . light))
    ("kanagawa-lotus" . (kanagawa-lotus . light)))
  "Map Omarchy theme names to Emacs themes and styles.")

;;;###autoload
(defun +theme-rotate-load-omarchy-theme (theme-name)
  "Load the Emacs theme mapped from Omarchy THEME-NAME."
  (interactive "sOmarchy theme: ")
  (let* ((theme-key (string-trim theme-name))
         (theme-style
          (cdr (assoc-string theme-key +theme-rotate-omarchy-theme-map t)))
         (theme (car theme-style))
         (style (cdr theme-style)))
    (when theme-style
      (+theme-rotate--set-current-theme-state theme style)
      (+theme-rotate-load-theme theme))))

(provide 'theme-rotate-omarchy)
