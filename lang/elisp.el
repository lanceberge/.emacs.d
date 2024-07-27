(use-package elisp-mode
  :straight (:type built-in)
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer))
