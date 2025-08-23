;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("Q" . #'+kmacro-record-or-end)
        ("Z" . #'+kmacro-call-named-macro)))

(defvar +kmacro--last-name nil)

(defun +kmacro-record-or-end (&optional n)
  "If a macro is being recorded, end it. If 'l' is input, call the last macro.
Otherwise, create a named macro with the char input."
  (interactive "p")
  (if defining-kbd-macro
      (progn
        (call-interactively #'kmacro-end-macro)
        (kmacro-name-last-macro +kmacro--last-name)
        (setq +kmacro--last-name nil))
    (let ((name (read-char "Name: ")))
      (if (= name ?l)
          (+kmacro--apply-to-region-or-lines n nil)
        (progn
          (call-interactively #'kmacro-start-macro)
          (setq +kmacro--last-name (+kmacro--get-name name)))))))

(defun +kmacro-call-named-macro (char-name &optional n)
  "Call the macro at name `char-name'. If 'l' is input, call the last macro."
  (interactive "cName: \np")
  (if (= char-name ?l)
      (+kmacro--apply-to-region-or-lines n nil)
    (let ((macro-symbol (+kmacro--get-name char-name)))
      (if (not (fboundp macro-symbol))
          (error "Macro at %c is not defined" char-name))
      (+kmacro--apply-to-region-or-lines n macro-symbol))))

(defun +kmacro--apply-to-region-or-lines (&optional n macro-symbol)
  "Apply function `macro-symbol'. If `macro-symbol' is nil, use the last macro.
If a region is active, it will be applied to the region. Otherwise, it will be applied
n times."
  (unless macro-symbol
    (kmacro-push-ring (kmacro-extract-lambda (symbol-function macro-symbol))))
  (with-undo-amalgamate
    (if (region-active-p)
        (call-interactively #'apply-macro-to-region-lines)
      (call-interactively #'kmacro-call-macro))))

(defun +kmacro--get-name (char)
  (intern (format "+kmacro--macro-%s" (char-to-string char))))
