;;  embark-this-buffer.el --- Embark whole buffer target & actions   -*- lexical-binding: t; -*-
;; https://github.com/oantolin/emacs-config/blob/4d19cbcf2bfa72f8a362243b40ce4a12b7a71b69/experiments/embark-this-buffer.el

(require 'embark)

;;;###autoload
(defun +bookmark-file ()
  (interactive)
  (when (buffer-file-name)
    (bookmark-set (file-name-nondirectory (buffer-file-name)) nil)))

(defvar-keymap this-buffer-map
  :doc "Commands to act on current file or buffer."
  :parent embark-general-map
  "RET" #'eval-buffer
  "l" #'load-file
  "e" #'eval-buffer
  "r" #'rename-file
  "d" #'delete-file
  "w" #'+file-name-kill-ring-save
  "W" #'write-file
  "v r" #'vc-rename-file
  "v d" #'vc-delete-file
  "n" #'diff-buffer-with-file           ; n for new
  "C-=" #'ediff-buffers
  "=" #'apheleia-format-buffer
  "b" #'+bookmark-file
  "$" #'ispell
  "!" #'shell-command
  "&" #'async-shell-command
  "x" #'embark-open-externally         ; useful for PDFs
  "c" #'copy-file
  "k" #'kill-current-buffer
  "z" #'bury-buffer
  "q" #'quit-window
  "|" #'embark-shell-command-on-buffer
  "g" #'+revert-buffer
  "p" #'pwd
  "h" #'mark-whole-buffer)

(cl-pushnew 'embark--allow-edit (alist-get 'write-file embark-target-injection-hooks))

(dolist (cmd '(previous-buffer next-buffer transpose-windows))
  (cl-pushnew cmd embark-repeat-actions))

;;;###autoload
(defun embark-on-this-buffer ()
  "Run `embark-act' on the current buffer."
  (interactive)
  (let ((embark-target-finders
         (list (lambda () (cons 'this-buffer (buffer-name)))))
        (embark-keymap-alist '((this-buffer . this-buffer-map))))
    (embark-act)))

(provide 'embark-this-buffer)
