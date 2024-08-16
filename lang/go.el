;;; -*- lexical-binding: t -*-
(use-package go-mode
  :defer t
  :config
  ;; https://github.com/meain/tree-surgeon/blob/master/tree-surgeon-go-error.el
;;;###autoload
  (defun tree-surgeon-go-error (&optional errformat)
    "Insert the return statement.
Pass in an `ERRFORMAT' of you want to override the default."
    (interactive)
    (let ((return-string (tree-surgeon-go-error--get-return-string
                          (or errformat tree-surgeon-go-error-format-default))))
      (if return-string
          (let ((start (progn (end-of-line) (newline) (point))))
            (insert "if err != nil {\n")
            (insert return-string)
            (insert "\n}")
            (indent-region start (point)))
        (message "Could not compute return statement"))))
  )
