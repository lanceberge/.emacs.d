;;; -*- lexical-binding: t -*-
(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

;;;###autoload
(defun +dumb-jump-xref-activate-unless-eglot ()
  "Return the dumb-jump xref backend unless eglot is managing the buffer."
  (unless (bound-and-true-p eglot--managed-mode)
    (dumb-jump-xref-activate)))

;;;###autoload
(defun +dumb-jump-maybe-enable ()
  "Enable dumb-jump as an xref backend in non-elisp `prog-mode' buffers."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (add-hook 'xref-backend-functions
              #'+dumb-jump-xref-activate-unless-eglot nil t)))

(add-hook 'prog-mode-hook #'+dumb-jump-maybe-enable)
