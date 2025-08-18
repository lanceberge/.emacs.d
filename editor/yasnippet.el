;;; -*- lexical-binding: t -*-
(use-package yasnippet ; snippets
  ;; https://joaotavora.github.io/yasnippet/snippet-development.html
  :defer 0.7
  :defer-incrementally (easymenu help-mode yasnippet-snippets)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-indent-line 'fixed)
  :general
  (my-leader-def
    "si" #'(yas-insert-snippet :which-key "insert")
    "sn" #'(yas-new-snippet :which-key "new")
    "sf" #'(yas-visit-snippet-file :which-key "find snippet")
    "sl" #'(yas-describe-tables :which-key "list")
    "sr" #'(yas-reload-all :which-key "reload"))
  ('yas-keymap
   "<tab>" #'yas-next-field)
  ('visual 'org-mode-map
           "ss" (defun +src-snippet () (interactive) (+expand-snippet "highlighted src")))
  ('visual 'prog-mode-map
           "st" (defun +try-catch-snippet () (interactive) (+expand-snippet "try-catch")))
  ('snippet-mode-map
   "C-c C-c" #'+yas-load-snippet-noconfirm)

  :config
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m)))))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :demand t
  :after yasnippet
  :init
  (yas--remove-template-by-uuid (yas--table-get-create 'emacs-lisp-mode) "kill-buffer"))

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
