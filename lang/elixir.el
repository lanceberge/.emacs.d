(defvar elixir-web-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ">") #'+maybe-close-tag)
    map)
  "Keymap for `elixir-web-mode'.")

(define-minor-mode elixir-web-mode
  "Elixir mode for web files."
  :lighter " Web"
  :keymap elixir-web-mode-map)

;;;###autoload
(defun maybe-elixir-web-mode ()
  "Enable `elixir-web-mode' if the module is '*Web.'"
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^defmodule [^.]*Web")
      (elixir-web-mode 1))))

;;;###autoload
(defun +maybe-close-tag ()
  "auto close html tags"
  (interactive)
  (insert ">")
  (save-excursion
    (backward-char)
    (when (looking-back "<\\([.:a-zA-Z_][a-zA-Z_.-]*\\)" (line-beginning-position))
      (let ((tag-name (match-string 1)))
        (unless (string-match-p "^/" tag-name)
          (forward-char)
          (insert (format "</%s>" tag-name))
          (backward-char (+ 2 (length tag-name))))))))

(use-package elixir-ts-mode
  :hook (elixir-ts-mode . maybe-elixir-web-mode))
