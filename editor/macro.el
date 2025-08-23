;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("Q" . #'+kmacro-record-or-end)
        ("Z" . #'+kmacro-call-named-macro)))

(defvar +kmacro--last-name nil)

(defun +kmacro-record-or-end ()
  "If a macro is being recorded, end it. If c is input, call the last macro.
Otherwise, create a named macro with the char input."
  (interactive)
  (if defining-kbd-macro
      (progn
        (call-interactively #'kmacro-end-macro)
        (kmacro-name-last-macro +kmacro--last-name))
    (let* ((name (read-char "Name: "))
           (macro-name (+kmacro--get-name name)))
      (if (= name ?c)
          (with-undo-amalgamate
            (call-interactively #'kmacro-call-macro))
        (progn
          (call-interactively #'kmacro-start-macro)
          (setq +kmacro--last-name macro-name))))))

(defun +kmacro-call-named-macro (char-name n)
  (interactive "cName: \np")
  (let ((macro-name (+kmacro--get-name char-name)))
    (with-undo-amalgamate
      (if (= char-name ?c)
          (call-interactively #'kmacro-call-macro)
        (dotimes (_ n)
          (funcall macro-name))))))

(defun +kmacro--get-name (char)
  (intern (format "+kmacro--macro-%s" (char-to-string char))))
