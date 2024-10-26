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
  ('emacs 'vterm-mode-map
          "C-u" (lambda () (interactive) (+vterm-copy-mode) (evil-scroll-up 0)))
  (my-leader-def
    :states 'insert
    "C-c" #'vterm--self-insert)

  ('normal
   "C-c C-t" #'+vterm-copy-mode)

  (my-leader-def
    :mode 'vterm-mode
    "C-t" #'+vterm-copy-mode)

  (my-leader-def
    "bv" #'(vterm :which-key "vterm")
    "ov" #'(+vterm :which-key "vterm"))
  :config
  (add-to-list 'vterm-keymap-exceptions "C-c C-t")
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-hook 'vterm-mode-hook (lambda ()
                               (display-line-numbers-mode -1))))

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
            (vterm-send-C-c)
            (vterm-send-escape)
            (vterm-send-string (concat "icd " (shell-quote-argument current-dir)))
            (vterm-send-return)))
      (vterm))))

;;;###autoload
(defun +vterm-copy-mode ()
  (interactive)
  (if (eq evil-state 'emacs)
      (progn
        (vterm--enter-copy-mode)
        (evil-normal-state))
    (progn
      (vterm--exit-copy-mode)
      (evil-emacs-state)
      (vterm-send-escape)
      (vterm-send-string "a"))))
