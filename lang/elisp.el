;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :general
  ('(normal insert) emacs-lisp-mode-map
   :prefix "C-c"
   "C-c" #'eval-buffer)
  :general
  ('emacs-lisp-mode-map
   [remap save-buffer] #'+elisp-format-and-check))

(use-package debug
  :ensure nil
  :commands
  (debug-on-entry))

(use-package edebug
  :ensure nil
  :defer t
  :general
  ;; TODO
  ;; (my-leader-def
  ;;   :keymaps 'emacs-lisp-mode-map
  ;;   "ed" #'(edebug-defun :which-key "debug function")
  ;;   "e SPC d" #'(edebug-remove-instrumentation :which-key "remove instrumentation"))
  )

;;;###autoload
(defun +elisp-format-and-check ()
  "Format buffer, check parens, and save if balanced."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (balanced-parens-p)
        (progn
          (save-buffer))
      (message "Parens are not balanced, saving canceled"))))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package puni
  :init
  (setq +open-chars '(?\( ?\{ ?\[))
  (setq +close-chars '(?\) ?\} ?\]))
  :general
  ('meow-normal-state-keymap
   "<" #'+slurp-or-barf-left
   ">" #'+slurp-or-barf-right))

;;;###autoload
(defun +slurp-or-barf-left (&optional N)
  (interactive "p")
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn
             (forward-char)
             (puni-slurp-backward N)
             (backward-sexp)
             (backward-char)))
          ((member char-at-point +close-chars)
           (puni-barf-forward N))
          (t
           (progn
             (forward-sexp)
             (puni-barf-forward N))))))

;;;###autoload
(defun +slurp-or-barf-right (&optional N)
  (interactive "p")
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn (forward-char) (puni-barf-backward N) (backward-char)))
          ((member char-at-point +close-chars)
           (puni-slurp-forward N))
          (t
           (progn
             (forward-sexp)
             (puni-slurp-forward N))))))
