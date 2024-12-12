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
  (my-leader-def
    :states 'insert
    "C-c" #'vterm--self-insert)

  ('normal 'vterm-mode-map
           "u" nil)
  (my-leader-def
    "bv" (defun +vterm-last () (interactive) (vterm) (evil-collection-vterm-append))
    "ov" #'(+vterm :which-key "vterm")
    "onv" (lambda (interactive) ()))
  :config
  (evil-collection-init 'vterm)
  (add-hook 'vterm-mode-hook (lambda ()
                               (display-line-numbers-mode -1))))

;;;###autoload
(defun +vterm (&optional arg dir)
  "Open or switch to a vterm buffer, changing directory if it's not equal to the cwd."
  (interactive "P")
  ;; If we specify arg - create one at arg. If there isn't a buffer "*vterm", create one
  (let* ((dir (or dir (expand-file-name default-directory)))
         (vterm-buffer
          (if arg
              (vterm arg)
            (or (seq-find
                 (lambda (buf)
                   (and (string-prefix-p "*vterm" (buffer-name buf))
                        (with-current-buffer buf
                          (string= dir (expand-file-name default-directory)))))
                 (buffer-list))
                (progn
                  (setq default-directory dir)
                  (vterm))))))
    (switch-to-buffer vterm-buffer)
    (with-current-buffer vterm-buffer
      (unless (string= dir default-directory)
        (vterm-send-string (concat "cd " dir))
        (vterm-send-return)))
    (evil-collection-vterm-append)))

(defun +vterm-new (&optional arg)
  "Create a new vterm buffer with optional numeric prefix ARG.
If ARG is nil, defaults to 1."
  (interactive "P")
  (let ((arg (or arg 1)))
    (+vterm arg)))

(defun +vterm-project ()
  (interactive)
  (+vterm nil (project-root (project-current t))))
