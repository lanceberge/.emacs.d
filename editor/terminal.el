;;; -*- lexical-binding: t -*-
(use-package terminal-here
  :general
  (my-leader-def
    "ot" #'terminal-here-launch :which-key "Launch terminal"))

(when IS-MAC
  (use-package tramp ; access remote files within emacs
    :ensure nil
    :defer t
    :ensure (:wait t)))

(when IS-LINUX
  (require 'tramp))

(use-package vterm
  :general
  ('normal vterm-mode-map
           "N" #'vterm--self-insert
           "R" #'vterm--self-insert)

  ('(normal insert) vterm-mode-map
   "C-l" #'vterm--self-insert
   "C-b" #'vterm--self-insert
   "C-c" #'vterm--self-insert)

  (my-leader-def
    :states 'insert
    "C-c" #'vterm--self-insert)

  (my-leader-def
    "ov" #'(+vterm :which-key "vterm"))
  :config
  (evil-collection-init 'vterm)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))


;;;###autoload
(defun +vterm ()
  "Open or switch to a vterm buffer, changing directory if it's not equal to the cwd."
  (interactive)
  (let ((vterm-buffer (get-buffer "*vterm*"))
        (current-dir (expand-file-name default-directory)))
    (if vterm-buffer
        (progn
          (switch-to-buffer vterm-buffer)
          (unless (string= current-dir (expand-file-name default-directory))
            (vterm-send-string (concat "cd " (shell-quote-argument current-dir)))
            (vterm-send-return)))
      (vterm)))
  (evil-collection-vterm-append))
