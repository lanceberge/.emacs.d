;;; -*- lexical-binding: t -*-
(use-package expand-region
  :bind
  ("C-=" . #'+expand-region)
  (:map meow-normal-state-keymap
        ("o" . #'+expand-region)
        ("O" . #'+expand-region-2))
  (:map meow-motion-state-keymap
        ("o" . #'+expand-region)
        ("O" . +expand-region-2))
  :config
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-ts-node))
  (with-eval-after-load 'yaml-mode
    (require 'yaml-mode-expansions)
    (require 'python)
    ;; the yaml expansion for some reason use python functions
    (add-hook 'yaml-mode-hook #'er/add-yaml-mode-expansions))

  (with-eval-after-load 'org-mode
    (require 'the-org-mode-expansions)
    (add-hook 'org-mode-hook #'er/add-org-mode-expansions))

  (with-eval-after-load 'nxml-mode
    (require 'nxml-mode-expansions)
    (add-hook 'nxml-mode-hook #'er/add-nxml-mode-expansions))

  (with-eval-after-load 'text-mode
    (require 'text-mode-expansions)
    (add-hook 'text-mode-hook #'er/add-text-mode-expansions)))

;;;###autoload
(defun +expand-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (let ((point-at-beginning (eq (point) (region-beginning))))
        (unless point-at-beginning
          (exchange-point-and-mark))
        (er/expand-region arg))
    (er/expand-region arg)))

;;;###autoload
(defun +expand-region-2 ()
  (interactive)
  (+expand-region 2)
  (exchange-point-and-mark))
