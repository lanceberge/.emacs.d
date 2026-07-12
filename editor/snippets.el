;;; -*- lexical-binding: t -*-
(use-package yasnippet ; snippets
  ;; https://joaotavora.github.io/yasnippet/snippet-development.html
  :hook
  ((prog-mode text-mode) . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-indent-line 'auto)
  :bind
  (:map +leader-map
        ("yi" . #'yas-insert-snippet)
        ("yn" . #'yas-new-snippet)
        ("yl" . #'yas-describe-tables)
        ("yr" . #'yas-reload-all))
  (:map yas-keymap
        ("<tab>" . #'yas-next-field))
  (:map snippet-mode-map
        ([remap save-buffer] . #'+yas-load-snippet-noconfirm)))

(use-package yasnippet-snippets
  :after yasnippet
  :init
  (yas--remove-template-by-uuid (yas--table-get-create 'emacs-lisp-mode) "kill-buffer")
  (yas--remove-template-by-uuid (yas--table-get-create 'elixir-mode) "do")
  (yas--remove-template-by-uuid (yas--table-get-create 'emacs-lisp-mode) "defun"))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind
  (:map +leader-map
        ("yf" . #'consult-yasnippet-visit-snippet-file)))

;;;###autoload
(defun +yas-load-snippet-noconfirm()
  "Load and save the snippet buffer and quit the window
while selecting the default table, file path, and not prompting
the user to save the buffer"
  (interactive)
  (unless yas--guessed-modes
    (setq-local yas--guessed-modes (yas--compute-major-mode-and-parents buffer-file-name)))
  (let ((template (yas-load-snippet-buffer (cl-first yas--guessed-modes) t)))
    (when (buffer-modified-p)
      (let ((default-directory (car (cdr (car (yas--guess-snippet-directories
                                               (yas--template-table template))))))
            (default-file-name (yas--template-name template)))
        (setq buffer-file-name (concat default-directory default-file-name))
        (rename-buffer default-file-name t)
        (save-buffer)))
    (quit-window t)))
