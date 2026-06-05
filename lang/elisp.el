;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . +elisp--maybe-setup-new-file)
  :bind
  (:map emacs-lisp-mode-map
        ([remap save-buffer] . +elisp-validate-balanced-parens))
  (:map +leader-map
        ("es" . #'eval-last-sexp)
        ("ee" . #'eval-expression)
        ("eb" . #'eval-buffer)
        ("ef" . #'eval-defun)))

(use-package debug
  :ensure nil
  :commands
  (debug-on-entry))

(use-package edebug
  :ensure nil)

;;;###autoload
(defun +elisp-validate-balanced-parens ()
  "Format buffer, check parens, and save if balanced."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (balanced-parens-p)
        (progn
          (save-buffer))
      (message "Parens are not balanced, saving canceled"))))

;;;###autoload
(defun +elisp--maybe-setup-new-file ()
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
    (if (bound-and-true-p yas-minor-mode)
        (+elisp--setup-new-file)
      (add-hook 'yas-minor-mode-hook #'+elisp--setup-new-file nil t))))

;;;###autoload
(defun +elisp--setup-new-file ()
  "Insert the `lexical-binding' header on new elisp files."
  (require 'yasnippet)
  (+yas-expand-snippet "lexical binding")
  (goto-char (point-max))
  (insert "\n"))

(add-hook 'find-file-not-found-functions #'+elisp--maybe-setup-new-file t)
