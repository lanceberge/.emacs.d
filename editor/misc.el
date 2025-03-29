(use-package fretboard
  :ensure (:host github :repo "skyefreeman/fretboard.el")
  :hook (fretboard-mode . turn-off-evil-mode)
  :commands (fretboard)
  :custom
  (fretboard-fret-count 12)
  (evil-set-initial-state 'fretboard-mode 'emacs)
  :general
  ('normal 'fretboard-mode-map
           "n" 'fretboard-next
           "p" 'fretboard-previous
           "k" 'fretboard-next-type
           "j" 'fretboard-previous-type
           "d" 'fretboard-toggle-display-type
           "t" 'fretboard-toggle-tuning-type
           "s" 'fretboard-switch-to-scale
           "c" 'fretboard-switch-to-chord
           "q" 'fretboard-quit-all))
