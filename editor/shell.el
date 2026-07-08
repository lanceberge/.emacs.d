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

;;;###autoload
(defun +eat-semi-char-tab ()
  "Switch Eat to semi-char mode and send a tab input event."
  (interactive)
  (eat-semi-char-mode)
  (+insert-mode 1)
  (eat-self-input 1 ?\t))

(use-package eat
  :custom
  (eat-enable-auto-line-mode t)
  (eat-tramp-shells '(("docker" . "/bin/bash")
                      ("ssh" . "/bin/bash")
                      ("scp" . "/bin/bash")
                      ("sshx" . "/bin/bash")
                      ("rsync" . "/bin/bash")))
  :hook
  (eat-eshell-exec . +eat-eshell-use-modal-cursor)
  (eshell-first-time-mode . eat-eshell-mode)
  :bind
  (:map eat-line-mode-map
        ("TAB" . +eat-semi-char-tab)))

;;;###autoload
(defun +eat-eshell-use-modal-cursor ()
  "Leave Eshell cursor shape under modal state control."
  (when (bound-and-true-p eat-terminal)
    (eat-term-set-parameter eat-terminal 'set-cursor-function #'ignore)))

(use-package eshell-extras
  :ensure (:type file :main "~/.emacs.d/lisp/eshell-extras.el" :files ("eshell-extras.el"))
  :after eshell
  :demand t
  :config
  (require 'esh-mode)
  (add-to-list 'eshell-expand-input-functions #'+eshell-expand-pipe-space)
  (add-to-list 'eshell-expand-input-functions #'+eshell-expand-less-pipe)
  (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

(use-package eshell
  :ensure nil
  :defer 4.0
  :init
  (add-to-list 'display-buffer-alist
               '("\\`\\*Eshell Command Output\\*\\'"
                 (+eshell-command-output-display-buffer)))
  :bind
  (:map +leader-map
        ("!" . #'eshell-command)
        ("ne" . #'eshell))
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-xtra)
  (add-hook 'eshell-mode-hook #'+eshell-outline-setup))

(use-package esh-mode
  :ensure nil
  :after eshell)

(use-package em-hist
  :ensure nil
  :after eshell
  :bind
  (:map eshell-mode-map
        ("C-M-i" . #'completion-at-point)
        ([remap consult-imenu] . #'consult-outline))
  (:map eshell-hist-mode-map
        ("M-r" . #'cape-history))
  :config
  (keymap-unset eshell-hist-mode-map "M-s"))

(use-package em-cmpl
  :ensure nil
  :bind
  (:map eshell-cmpl-mode-map
        ("C-M-i" . #'completion-at-point)))

(with-eval-after-load 'no-littering
  (setq eshell-aliases-file (expand-file-name "eshell-aliases" user-emacs-directory))
  (add-to-list 'auto-mode-alist
               (cons (concat "\\`" (regexp-quote eshell-aliases-file) "\\'")
                     #'+eshell-alias-mode)))

(use-package eshell-syntax-highlighting ;; visually show unsupported commands
  :hook (eshell-mode . eshell-syntax-highlighting-mode))
