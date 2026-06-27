;;; -*- lexical-binding: t -*-
(use-package tramp ; access remote files within emacs
  :ensure nil)

;;;###autoload
(defun +tramp--list-ssh-hosts ()
  "Return host aliases from ~/.ssh/config."
  (let ((config (expand-file-name "~/.ssh/config"))
        hosts)
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (while (re-search-forward "^Host \\(.*\\)$" nil t)
          (push (match-string 1) hosts))))
    (nreverse hosts)))

;;;###autoload
(defun +tramp-find-file ()
  "Pick a host from ~/.ssh/config and run `find-file' rooted there via TRAMP."
  (interactive)
  (let* ((host (completing-read "SSH host: " (+tramp--list-ssh-hosts) nil t))
         (default-directory (format "/ssh:%s:" host)))
    (call-interactively #'find-file)))

(use-package eat
  :hook
  (eat-eshell-exec . +eat-eshell-use-modal-cursor)
  (eshell-first-time-mode . eat-eshell-mode))

;;;###autoload
(defun +eat-eshell-use-modal-cursor ()
  "Leave Eshell cursor shape under modal state control."
  (when (bound-and-true-p eat-terminal)
    (eat-term-set-parameter eat-terminal 'set-cursor-function #'ignore)))

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
    (eshell-bol)
    (when (fboundp '+insert-mode)
      (+insert-mode 1))))

;;;###autoload
(defun +eshell-outline-setup ()
  "Configure outline navigation for Eshell prompts."
  (setq-local outline-regexp eshell-prompt-regexp)
  (add-hook 'consult-after-jump-hook #'+eshell-outline-after-jump nil t))

(use-package eshell
  :ensure nil
  :defer 0.7
  :commands
  (eshell project-eshell eshell-command)
  :init
  (add-to-list 'display-buffer-alist
               '("\\`\\*Eshell Command Output\\*\\'"
                 (+eshell-command-output-display-buffer)))
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-xtra)
  (add-hook 'eshell-mode-hook #'+eshell-outline-setup))

;;;###autoload
(defun +eshell-expand-less-pipe (beg end)
  "Rewrite literal \"| less\" pipes in Eshell input to \"*| less\"."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (search-forward "| less" end-marker t)
        (unless (eq (char-before (match-beginning 0)) ?*)
          (replace-match "*| less" t t))))))

(use-package esh-mode
  :ensure nil
  :after eshell
  :bind
  :config
  (add-to-list 'eshell-expand-input-functions #'+eshell-expand-less-pipe)
  (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

(use-package em-hist
  :ensure nil
  :after eshell
  :bind
  (:map eshell-mode-map
        ("C-M-i" . #'completion-at-point)
        ([remap consult-imenu] . #'consult-outline))
  (:map eshell-hist-mode-map
        ("M-r" . #'cape-history)))

(use-package em-cmpl
  :ensure nil
  :bind
  (:map eshell-cmpl-mode-map
        ("C-M-i" . #'completion-at-point)))

;;;###autoload
(define-derived-mode +eshell-alias-mode fundamental-mode "Eshell Alias"
  "Major mode for editing Eshell aliases."
  (require 'eshell)
  (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))

(with-eval-after-load 'no-littering
  (setq eshell-aliases-file (expand-file-name "eshell-aliases" user-emacs-directory))
  (add-to-list 'auto-mode-alist
               (cons (concat "\\`" (regexp-quote eshell-aliases-file) "\\'")
                     #'+eshell-alias-mode)))

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
  (rename-buffer (project-prefixed-buffer-name "eshell") t))

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
