;;  embark-this-buffer.el --- Embark whole buffer target & actions   -*- lexical-binding: t; -*-
;; https://github.com/oantolin/emacs-config/blob/4d19cbcf2bfa72f8a362243b40ce4a12b7a71b69/experiments/embark-this-buffer.el

(require 'embark)

;;;###autoload
(defun +bookmark-file ()
  (interactive)
  (when (buffer-file-name)
    (bookmark-set (file-name-nondirectory (buffer-file-name)) nil)))

;;;###autoload
(defun +embark-this-buffer-move-to-window ()
  "Move the current buffer to an ace-selected window."
  (interactive)
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+embark-this-buffer--ensure-dispatch-window)
    (aw-move-window (aw-select " Ace - Move Buffer"))))

;;;###autoload
(defun +embark-this-buffer-open-in-new-window ()
  "Open the current buffer in a newly-created window."
  (interactive)
  (let ((buffer (current-buffer)))
    (select-window (+window-split-new))
    (switch-to-buffer buffer)))

;;;###autoload
(defun +embark-this-buffer--ensure-dispatch-window ()
  "Create a second window when ace-window would have no target choice."
  (when (one-window-p)
    (or (split-window-sensibly)
        (split-window-right))))

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
  "N" #'+embark-this-buffer-open-in-new-window
  "o" #'+embark-this-buffer-move-to-window
  "z" #'bury-buffer
  "q" #'quit-window
  "|" #'embark-shell-command-on-buffer
  "g" #'+revert-buffer
  "p" #'pwd
  "h" #'mark-whole-buffer)

(keymap-set this-buffer-map "N" #'+embark-this-buffer-open-in-new-window)
(keymap-set this-buffer-map "o" #'+embark-this-buffer-move-to-window)

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
