;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("M-r" . #'+kmacro-record-or-end-unnamed)
        ("M-m" . #'+kmacro-execute)
        ("M-R" . #'+kmacro-record-or-end)
        ("M-M" . #'+kmacro-call-named-macro)))

(defvar +kmacro--last-name nil)

;;;###autoload
(defun +kmacro-record-or-end-unnamed ()
  (interactive)
  ;; execute
  (if defining-kbd-macro
      (call-interactively #'kmacro-end-macro)
    (call-interactively #'kmacro-start-macro)))

;;;###autoload
(defun +kmacro-execute (&optional arg)
  (interactive "p")
  (with-undo-amalgamate
    (if (region-active-p)
        (progn
          (call-interactively #'apply-macro-to-region-lines)
          (deactivate-mark))
      (kmacro-call-macro arg))))

;;;###autoload
(defun +kmacro-record-or-end (&optional n char)
  "If a macro is being recorded, end it. If 'l' is input, call the last macro.
Otherwise, create a named macro with the char input."
  (interactive "p")
  (if defining-kbd-macro
      (progn
        (call-interactively #'kmacro-end-macro)
        (kmacro-name-last-macro +kmacro--last-name)
        (setq +kmacro--last-name nil))
    (let ((name (or char (read-char "Name: "))))
      (if (= name ?l)
          (+kmacro--apply-to-region-or-lines n nil)
        (progn
          (call-interactively #'kmacro-start-macro)
          (setq +kmacro--last-name (+kmacro--get-name name)))))))

;;;###autoload
(defun +kmacro-call-named-macro (char-name &optional n)
  "Call the macro at name `char-name'. If 'l' is input, call the last macro."
  (interactive "cName: \np")
  (if (= char-name ?l)
      (+kmacro--apply-to-region-or-lines n nil)
    (let ((macro-symbol (+kmacro--get-name char-name)))
      (if (not (fboundp macro-symbol))
          (error "Macro at %c is not defined" char-name))
      (+kmacro--apply-to-region-or-lines n macro-symbol))))

;;;###autoload
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

;;;###autoload
(defun +kmacro--get-name (char)
  (intern (format "+kmacro--macro-%s" (char-to-string char))))
