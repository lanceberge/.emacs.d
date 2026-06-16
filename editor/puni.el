;;; -*- lexical-binding: t -*-
(use-package puni)

(use-package puni-extensions
  :ensure (:type file :main "~/.emacs.d/packages/puni-extensions.el")
  :bind
  (:map +insert-mode-map
        ("C-(" . #'+puni-slurp-or-barf-left)
        ("C-)" . #'+puni-slurp-or-barf-right))
  (:map +normal-mode-map
        ("(" . +puni-slurp-or-barf-left)
        (")" . +puni-slurp-or-barf-right)))
