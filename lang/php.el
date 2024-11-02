(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  ;; :hook
  ;; (php-mode . (lambda ()
  ;;               (add-hook 'after-save-hook #'php-cs-fixer-fix-current nil t)))
  )

;; ;;;###autoload
;; (defun php-cs-fixer-fix-current ()
;;   "Run PHP CS Fixer on the current file silently."
;;   (interactive)
;;   (when (and buffer-file-name
;;              (string-match "\\.php\\'" buffer-file-name))
;;     (let ((command (concat "~/.config/composer/vendor/bin/php-cs-fixer fix "
;;                            (shell-quote-argument buffer-file-name)))
;;           (current-point (point)))
;;       (call-process-shell-command command nil 0)
;;       (run-with-timer 0.1 nil
;;                       (lambda ()
;;                         (revert-buffer nil t t)
;;                         (goto-char current-point))))))
