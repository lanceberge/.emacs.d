;;; -*- lexical-binding: t -*-

(use-package puni)

(use-package puni-extensions
  :ensure (:type file :main "~/.emacs.d/packages/puni-extensions.el")
  :bind
  (:map +normal-mode-map
        ("(" . +puni-slurp-or-barf-left)
        (")" . +puni-slurp-or-barf-right)))
