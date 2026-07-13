;;; -*- lexical-binding: t -*-
(use-package treesit
  :unless IS-WORK2
  :ensure nil
  :after yasnippet
  :config
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)

    (let* ((source-mode (car mapping))
           (target-mode (cdr mapping))
           (source-mode-hook (intern (concat (symbol-name source-mode) "-hook")))
           (target-mode-hook (intern (concat (symbol-name target-mode) "-hook"))))

      (add-hook source-mode-hook
                (lambda ()
                  (yas-activate-extra-mode target-mode)))
      (add-hook target-mode-hook
                (lambda ()
                  (yas-activate-extra-mode source-mode))))))
