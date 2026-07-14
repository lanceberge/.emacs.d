;;; -*- lexical-binding: t -*-
(use-package tramp ; access remote files within emacs
  :ensure nil
  :bind
  (:map search-map
        ("M-s" . #'+tramp-find-file)))

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
  :custom
  (eat-enable-auto-line-mode t)
  (eat-enable-shell-prompt-annotation t)
  (eat-shell-prompt-annotation-success-margin-indicator "")
  (eat-tramp-shells '(("docker" . "/bin/bash")
                      ("ssh" . "/bin/bash")
                      ("scp" . "/bin/bash")
                      ("sshx" . "/bin/bash")
                      ("rsync" . "/bin/bash")))
  :hook
  (eshell-first-time-mode . eat-eshell-mode)
  :bind
  (:map +leader-map
        ("ns" . #'eat))
  (:map eat-line-mode-map
        ("M-r" . #'consult-history))
  :config
  (add-to-list 'consult-mode-histories
               '(eat-mode eat--line-input-ring eat--line-input-ring-index)))

(use-package eat-extras
  :ensure (:type file :main "~/.emacs.d/lisp/eat-extras.el" :files ("eat-extras.el"))
  :after key-chord
  :hook
  (eat-eshell-exec . +eat-eshell-use-modal-cursor)
  :bind
  (:map eat-line-mode-map
        ("TAB" . +eat-semi-char-tab)
        ([remap eat-semi-char-mode] . #'+modal-eat-semi-char-mode-insert))
  ;; (:map +llm-map
  ;;       ("c" . #'+eat-llm))
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (add-hook '+insert-mode-hook '+eat-insert-mode-reevaluate nil t)))
  (key-chord-define eat-semi-char-mode-map "jk" #'+eat-line-mode-normal)
  (advice-add 'eat--set-cmd
              :before #'+eat--record-shell-command-in-line-history)
  (advice-add 'eat--pre-cmd :after #'+eat--command-started)
  (advice-add 'eat--post-prompt :after #'+eat--command-finished)
  (+modal-create-insert-function eat-semi-char-mode)
  (setq +ghostel-llm-command (if IS-WORK "claude" "pi"))
  (setq +ghostel-llm-buffer-base-name (if IS-WORK "Claude" "Pi")))

(use-package ghostel
  :unless IS-WORK2
  :custom
  (ghostel-ignore-cursor-change t)
  (ghostel-initial-input-mode 'line)
  (ghostel-line-mode-bash-completion-prespawn t)
  (ghostel-query-before-killing 'auto)
  :bind
  (:map +leader-map
        ("nt" . #'ghostel))
  (:map project-prefix-map
        ("t" . #'ghostel-project)
        ("T" . #'ghostel-project-list-buffers)))

(use-package ghostel-extras
  :ensure (:type file :main "~/.emacs.d/lisp/ghostel-extras.el" :files ("ghostel-extras.el"))
  :unless IS-WORK2
  :after key-chord
  :hook
  (ghostel-mode . +ghostel-override-insert-mode-key-chords)
  (ghostel-mode . +ghostel-tramp-initial-input-mode)
  :bind
  (:map ghostel-line-mode-map
        ("<tab>" . #'+ghostel-semi-char-tab)
        ("M-r" . #'+ghostel-consult-history)
        ([remap +modal-end-of-buffer-insert] . #'+ghostel-reset-point)
        ("C-c C-j" . #'+modal-ghostel-semi-char-mode-insert))
  (:map ghostel-semi-char-mode-map
        ("RET" . #'+ghostel-semi-char-return)
        ("<return>" . #'+ghostel-semi-char-return)
        ("M-r" . #'+ghostel-consult-history))
  (:map +llm-map
        ("i" . #'+ghostel-llm))
  :config
  (add-hook 'ghostel-mode-hook
            (lambda ()
              (add-hook '+insert-mode-hook '+ghostel-insert-mode-reevaluate nil t)))
  (setq +ghostel-llm-command (if IS-WORK "claude" "pi"))
  (setq +ghostel-llm-buffer-base-name (if IS-WORK "Claude" "Pi"))
  (+modal-create-insert-function ghostel-semi-char-mode)
  (key-chord-define ghostel-semi-char-mode-map "jk" #'+ghostel-line-mode-normal)

  ;; better auto line -> semi char switching
  (add-hook 'ghostel-command-start-functions #'+ghostel-auto-semi-char-mode)
  (add-hook 'ghostel-command-finish-functions #'+ghostel-auto-line-mode)
  (advice-add 'ghostel--line-mode-enter :after #'+ghostel-use-modal-cursor)
  (add-to-list 'consult-mode-histories
               '(ghostel-mode
                 ghostel--line-mode-history
                 ghostel--line-mode-history-index
                 ghostel-beginning-of-input-or-line)))

(use-package eshell
  :custom
  (eshell-prefer-lisp-functions t)
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

(use-package eshell-extras
  :ensure (:type file :main "~/.emacs.d/lisp/eshell-extras.el" :files ("eshell-extras.el"))
  :after eshell
  :demand t
  :config
  (require 'esh-mode)
  (add-to-list 'eshell-expand-input-functions #'+eshell-expand-pipe-space)
  (add-to-list 'eshell-expand-input-functions #'+eshell-expand-less-pipe)
  (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

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
