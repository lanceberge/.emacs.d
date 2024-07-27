;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :straight (:type built-in)
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer)
  ('emacs-lisp-mode-map
   [remap evil-write] #'+elisp-format-and-check))


(defun +elisp-format-and-check ()
  "Format buffer, check parens, and save if balanced."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (balanced-parens-p)
        (progn
          (format-all-buffer)
          (save-buffer))
      (message "Parens are not balanced, saving canceled"))))
