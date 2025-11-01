;;; -*- lexical-binding: t -*-
(use-package expand-region
  :hook
  (yaml-mode . er/add-yaml-mode-expansions)
  :bind
  (:map meow-normal-state-keymap
        ("o" . #'+expand-region)
        ("O" . #'+expand-region-2))
  (:map meow-insert-state-keymap
        ("C-=" . #'+expand-region))
  (:map meow-motion-state-keymap
        ("o" . #'+expand-region)
        ("O" . +expand-region-2))
  :config
  (eval-after-load 'yaml-ts-mode '(require 'yaml-mode-expansions))
  (eval-after-load 'yaml-ts-mode '(require 'python)) ;; the yaml expansion for some reason use python functions
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-ts-node)))

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
