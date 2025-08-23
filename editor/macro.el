;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("Q" . #'+kmacro-record-or-end)
        ("Z" . #'+kmacro-call-named-macro)))

(defvar +kmacro--last-name nil)

(defun +kmacro-record-or-end (&optional n)
  "If a macro is being recorded, end it. If c is input, call the last macro.
Otherwise, create a named macro with the char input."
  (interactive "p")
  (if defining-kbd-macro
      (progn
        (call-interactively #'kmacro-end-macro)
        (kmacro-name-last-macro +kmacro--last-name))
    (let* ((name (read-char "Name: "))
           (macro-name (+kmacro--get-name name)))
      (if (= name ?l)
          (with-undo-amalgamate
            (if (region-active-p)
                (call-interactively #'apply-macro-to-region-lines)
              (dotimes (_ n)
                (call-interactively #'kmacro-call-macro))))
        (progn
          (call-interactively #'kmacro-start-macro)
          (setq +kmacro--last-name macro-name))))))

(defun +kmacro-call-named-macro (char-name &optional n)
  (interactive "cName: \np")
  (let ((macro-symbol (+kmacro--get-name char-name)))
    (with-undo-amalgamate
      (if (= char-name ?l)
          (if (region-active-p)
              (call-interactively #'apply-macro-to-region-lines)
            (call-interactively #'kmacro-call-macro))
        (if (region-active-p)
            (+kmacro--apply-to-region macro-symbol)
          (dotimes (_ n)
            (funcall macro-symbol)))))))

(defun +kmacro--apply-to-region (macro-symbol)
  (if (not (region-active-p))
      (error "No active region"))
  (if (not (fboundp macro-symbol))
      (error "Symbol %s is not a defined function or macro" macro-symbol))
  (kmacro-push-ring (kmacro-extract-lambda (symbol-function macro-symbol)))
  (call-interactively #'apply-macro-to-region-lines))

(defun +kmacro--get-name (char)
  (intern (format "+kmacro--macro-%s" (char-to-string char))))
