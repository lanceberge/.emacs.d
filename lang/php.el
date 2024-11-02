(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :hook
  (php-mode . +php-mode)
  :general
  (my-leader-def
    "=" (lambda () (interactive)
          (save-buffer)
          (php-cs-fixer-fix-current))))

(defun +php-mode ()
  (add-hook 'after-save-hook #'php-cs-fixer-fix-current nil t)
  (format-all-mode -1))

;;;###autoload
(defun php-cs-fixer-fix-current ()
  "Run PHP CS Fixer on the current file silently."
  (interactive)
  (when (and buffer-file-name
             (string-match "\\.php\\'" buffer-file-name))
    (let ((command (concat "~/.config/composer/vendor/bin/php-cs-fixer fix --using-cache=no "
                           (shell-quote-argument buffer-file-name)))
          (current-point (point)))
      (call-process-shell-command command nil 0)
      (run-with-timer 0.1 nil
                      (lambda ()
                        (revert-buffer nil t t)
                        (goto-char current-point))))))
