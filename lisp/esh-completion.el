;;; esh-completion.el --- Bash completions for Eshell -*- lexical-binding: t -*-

(defgroup esh-completion nil
  "Bash completions for Eshell."
  :group 'eshell)

(defcustom +esh-completion-script
  (expand-file-name "list_completions.sh" user-emacs-directory)
  "Script used to fetch Bash completion candidates."
  :type 'file)

;;;###autoload
(defun +esh-completion-mode ()
  "Enable Bash completion candidates in Eshell."
  (add-hook 'completion-at-point-functions #'+esh-completion-capf 50 t))

;;;###autoload
(defun +esh-completion-capf ()
  "Complete the current Eshell token using Bash completions."
  (let* ((line-start (save-excursion
                       (eshell-bol)
                       (point)))
         (line-end (save-excursion
                     (end-of-line)
                     (point)))
         (token-start (save-excursion
                        (skip-chars-backward "^ \t\r\n" line-start)
                        (point)))
         (line (buffer-substring-no-properties line-start (min (point) line-end))))
    (list token-start
          (point)
          (+esh-completion--table line default-directory)
          :exclusive 'no)))

;;;###autoload
(defun +esh-completion--table (line directory)
  "Return a completion table for Bash completions of LINE in DIRECTORY."
  (let (candidates)
    (lambda (string predicate action)
      (unless candidates
        (setq candidates (+esh-completion--candidates line directory)))
      (complete-with-action action candidates string predicate))))

;;;###autoload
(defun +esh-completion--candidates (line directory)
  "Return Bash completion candidates for LINE in DIRECTORY."
  (when (file-executable-p +esh-completion-script)
    (let ((default-directory directory))
      (condition-case nil
          (process-lines +esh-completion-script line)
        (error nil)))))

(provide 'esh-completion)
