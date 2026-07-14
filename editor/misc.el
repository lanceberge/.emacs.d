;;; -*- lexical-binding: t -*-
(use-package fretboard
  :unless IS-WORK
  :ensure (:host github :repo "skyefreeman/fretboard.el")
  :commands (fretboard)
  :custom
  (fretboard-fret-count 12)
  :bind
  (:map fretboard-mode-map
        ("n" . fretboard-next)
        ("p" . fretboard-previous)
        ("k" . fretboard-next-type)
        ("j" . fretboard-previous-type)
        ("d" . fretboard-toggle-display-type)
        ("t" . fretboard-toggle-tuning-type)
        ("s" . fretboard-switch-to-scale)
        ("c" . fretboard-switch-to-chord)
        ("q" . fretboard-quit-all)))

(use-package  know-your-http-well
  :commands
  (http-header
   http-method
   http-relation
   http-status-code
   media-type))
