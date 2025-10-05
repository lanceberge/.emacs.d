;;; -*- lexical-binding: t -*-
(use-package kmacro
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("M-r" . #'+kmacro-record-or-end)
        ("M-m" . #'+kmacro-execute-dwim)
        ("M-M" . #'+kmacro-call-named-macro)))

(defvar +kmacro-last-register nil)

;;;###autoload
(defun +kmacro-record-or-end (arg)
  "Record or end a macro. It will be saved as a function named based on the prefix arg if it is supplied
to either record or end."
  (interactive "P")
  (if defining-kbd-macro
      (let ((register (or arg +kmacro-last-register)))
        (end-kbd-macro 1 #'kmacro-loop-setup-function)
        (if (and last-kbd-macro (= (length last-kbd-macro) 0))
            (progn
              (setq last-kbd-macro nil)
              (message "Ignore empty macro")
              (while (and (null last-kbd-macro) kmacro-ring)
                (kmacro-pop-ring1)))
          ;; if non-empty macro
          (when register
            (kmacro-name-last-macro (+kmacro--get-name (string-to-char (number-to-string register))))
            (message "Recorded macro %d" register)
            (setq +kmacro-last-register nil))))
    (call-interactively #'kmacro-start-macro)
    (setq +kmacro-last-register arg)))

;;;###autoload
(defun +kmacro-execute-dwim (&optional arg)
  "Execute a macro either ARG times or apply it to the region if one is active."
  (interactive "p")
  (if defining-kbd-macro
      (call-interactively #'kmacro-end-macro))
  (with-undo-amalgamate
    (if (region-active-p)
        (progn
          (call-interactively #'apply-macro-to-region-lines)
          (deactivate-mark))
      (kmacro-call-macro arg))))

;;;###autoload
(defun +kmacro-call-named-macro (char-name &optional n)
  "Call the macro at name `char-name'. This is created by a prefix arg being supplied to
`+kmacro-record-or-end'."
  (interactive "cName: \np")
  (let ((macro-symbol (+kmacro--get-name char-name)))
    (if (not (fboundp macro-symbol))
        (error "Macro at %c is not defined" char-name))
    (+kmacro--apply-to-region-or-lines n macro-symbol)))

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
  (intern (format "+kmacro--macro-%c" char)))
