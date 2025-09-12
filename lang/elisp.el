;;; -*- lexical-binding: t -*-
(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ([remap save-buffer] . +elisp-format-and-check))
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
  (setq +open-chars '(?\( ?\{ ?\[ ?\"))
  (setq +close-chars '(?\) ?\} ?\] ?\"))
  (setq +all-chars (concat (apply #'string +open-chars)
                           (apply #'string +close-chars)))
  :bind (:map meow-normal-state-keymap
              ("<" . +slurp-or-barf-left)
              (">" . +slurp-or-barf-right)))

;;;###autoload
(defun +slurp-or-barf-left (&optional N)
  (interactive "p")
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn
             (forward-char)
             (puni-slurp-backward N)
             (search-backward (char-to-string char-at-point))))
          ((member char-at-point +close-chars)
           (puni-barf-forward N))
          (t
           (progn
             (skip-chars-forward (concat "^" +all-chars))
             (+slurp-or-barf-left N))))))

;;;###autoload
(defun +slurp-or-barf-right (&optional N)
  (interactive "p")
  (let ((char-at-point (char-after (point))))
    (cond ((member char-at-point +open-chars)
           (progn
             (forward-char)
             (puni-barf-backward N)
             (backward-char)))
          ((member char-at-point +close-chars)
           (progn
             (puni-slurp-forward N)
             (search-forward (char-to-string char-at-point))
             (backward-char)))
          (t
           (progn
             (skip-chars-forward (concat "^" +all-chars))
             (+slurp-or-barf-right))))))
