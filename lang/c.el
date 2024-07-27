;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :straight (:type built-in)
  :hook (c-mode . +cc/company-backends)
  :mode ("\\.cu\\'" . c-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :general
  ('c-mode-base-map
   "M-;" #'my/append-semicolon))

(use-package gdb-mi
  :straight (:type built-in)
  :general
  ('c-mode-map
   "C-c g" #'(gdb :which-key "gdb")))

;; TODO
;;;###autoload
(defun +cc/company-backends ()
  (defun prog-mode-company-backends ()
    (add-to-list 'company-backends '(company-capf
                                     company-dabbrev-code
                                     company-keywords)))

  (add-hook 'prog-mode-hook 'prog-mode-company-backends)
  "company backends for c-mode"
  (add-to-list 'company-backends
               '(company-clang
                 company-cmake
                 )))
