;;; eshell-extras.el --- Extra Eshell helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'esh-io)
(require 'project)
(require 'recentf)
(require 'eshell)

;;;###autoload
(defun +eshell-command-output-quit ()
  "Kill the current Eshell command output buffer and delete its window."
  (interactive)
  (quit-window t))

;;;###autoload
(define-minor-mode +eshell-command-output-mode
  "Minor mode for Eshell command output buffers."
  :keymap (define-keymap
            "q" #'+eshell-command-output-quit))

;;;###autoload
(defun +eshell-command-output-display-buffer (buffer _alist)
  "Enable `+eshell-command-output-mode' in BUFFER."
  (with-current-buffer buffer
    (+eshell-command-output-mode 1))
  nil)

;;;###autoload
(defun +eshell-outline-after-jump ()
  "Move to Eshell input after a `consult-outline' jump."
  (when (derived-mode-p 'eshell-mode)
    (end-of-line)
    (beginning-of-line)
    (when (fboundp '+insert-mode)
      (+insert-mode 1))))

;;;###autoload
(defun +eshell-outline-setup ()
  "Configure outline navigation for Eshell prompts."
  (setq-local outline-regexp eshell-prompt-regexp)
  (add-hook 'consult-after-jump-hook #'+eshell-outline-after-jump nil t))

;;;###autoload
(defun +eshell-expand-less-pipe (beg end)
  "Rewrite literal \"|less\" or \"| less\" pipes in Eshell input to \"*| less\"."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "|[[:space:]]*less\\_>" end-marker t)
        (unless (eq (char-before (match-beginning 0)) ?*)
          (replace-match "*| less" t t))))))

;;;###autoload
(defun +eshell-expand-pipe-space (beg end)
  "Rewrite \"|COMMAND\" pipes in Eshell input to \"| COMMAND\"."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "|\\([^[:space:]|&]\\)" end-marker t)
        (replace-match "| \\1" t)))))

;;;###autoload
(define-derived-mode +eshell-alias-mode fundamental-mode "Eshell Alias"
  "Major mode for editing Eshell aliases."
  (require 'eshell)
  (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))

;;;###autoload
(defun eshell/e (&rest files)
  "Open FILES in another window."
  (mapc #'find-file-other-window (flatten-tree files))
  nil)

(cl-defmethod eshell-get-target :after ((raw-target buffer) &optional _mode)
  "Display buffer redirection targets in another window."
  (display-buffer raw-target '((display-buffer-reuse-window
                                display-buffer-pop-up-window)
                               (inhibit-same-window . t))))

;;;###autoload
(defun eshell/rg (&rest args)
  "Run rg from Eshell in a `grep-mode' buffer."
  (eshell-grep "rg"
               (append '("--no-heading"
                         "--line-number"
                         "--column"
                         "--color=never"
                         "--with-filename")
                       args)))

;;;###autoload
(defun eshell/z (&optional regexp)
  "Navigate to a directory from `recentf-list' in Eshell.

With REGEXP, jump to the first recentf directory matching REGEXP.
Without REGEXP, choose from recentf directories using `completing-read'."
  (let ((eshell-dirs (+eshell-recentf-directories)))
    (eshell/cd
     (if regexp
         (or (catch 'match
               (dolist (dir eshell-dirs)
                 (when (string-match-p regexp dir)
                   (throw 'match dir))))
             (user-error "No recentf directory matching %S" regexp))
       (completing-read "cd: " eshell-dirs nil t)))))

;;;###autoload
(defun eshell/zp ()
  "Navigate to a project selected by `project-prompter' in Eshell."
  (require 'project)
  (eshell/cd (funcall project-prompter))
  (rename-buffer (project-prefixed-buffer-name "eshell") t)
  nil)

;;;###autoload
(defun +eshell-recentf-directories ()
  "Return unique abbreviated directories from `recentf-list'."
  (require 'recentf)
  (unless recentf-mode
    (recentf-mode 1))
  (delete-dups
   (delq nil
         (mapcar (lambda (file)
                   (when-let ((dir (if (directory-name-p file)
                                       file
                                     (file-name-directory file))))
                     (abbreviate-file-name
                      (file-name-as-directory dir))))
                 recentf-list))))

(provide 'eshell-extras)
