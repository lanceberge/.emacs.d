;;; -*- lexical-binding: t -*-
(use-package expand-region
  :bind
  ("C-=" . #'+expand-region)
  :hook
  (yaml-mode . er/add-yaml-mode-expansions)
  (nxml-mode . er/add-nxml-mode-expansions)
  (add-hook . er/add-org-mode-expansions)
  (add-hook . er/add-text-mode-expansions)
  :init
  (with-eval-after-load 'yaml-mode
    (require 'yaml-mode-expansions)
    (require 'python))

  (with-eval-after-load 'org-mode
    (require 'the-org-mode-expansions))

  (with-eval-after-load 'nxml-mode
    (require 'nxml-mode-expansions))

  (with-eval-after-load 'text-mode
    (require 'text-mode-expansions))
  :config
  (setq-default er/try-expand-list
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
