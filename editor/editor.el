;;; -*- lexical-binding: t -*-
(use-package format-all ; format code functions
  :hook
  ((prog-mode
    web-mode
    json-mode
    svelte-mode) . format-all-mode)
  :custom
  (format-all-show-errors t)
  :bind
  (:map +leader-map
        ("=" . #'+format/buffer))
  :config
  (setq-default format-all-formatters format-all-default-formatters))

(use-package goto-chg
  :disabled
  :bind
  (:map meow-normal-state-keymap
        ("C-p" . goto-last-change)
        ("C-n" . goto-last-change-reverse)))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  (embark-confirm-act-all nil)
  (embark-quit-after-action nil)
  :bind
  (:map meow-normal-state-keymap
        ("M-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map minibuffer-mode-map
        ("M-." . #'embark-act)
        ("M-," . #'+embark-select)
        ("M-a" . #'embark-act-all)
        ("M-r" . #'embark-become)
        ("C-c C-e" . #'embark-export))
  (:map embark-symbol-map
        ("s" . #'+project-replace-regex)
        ("h" . #'helpful-symbol))
  (:map meow-motion-mode-hook
        ("M-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-become-help-map
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("k" . #'helpful-key))
  (:map embark-region-map
        ("s" . #'+project-replace-regex))
  (:map embark-general-map
        ("'" . #'embark-dwim)
        ("f" . #'consult-ripgrep)
        ("d" . #'embark-find-definition)
        ("/" . #'consult-line)
        ("g" . #'goolge-this-word))
  (:map embark-collect-mode-map
        ("F" . #'consult-focus-lines)
        ([remap meow-bounds-of-thing] . #'embark-act))
  (:map embark-identifier-map
        ("SPC" . #'eglot-code-actions))
  :config
  ;; Noconform actions embark
  (setq embark-pre-action-hooks
        (cl-remove-if (lambda (hook)
                        (eq (car (cdr hook)) 'embark--confirm))
                      embark-pre-action-hooks)))

;;;###autoload
(defun +embark-select ()
  (interactive)
  (let ((vertico-cycle nil))
    (embark-select)
    (vertico-next)))

(use-package dot-mode
  :ensure (:host github :repo "wyrickre/dot-mode")
  :hook ((prog-mode text-mode) . dot-mode-on)
  :custom
  (dot-mode-ignore-undo t)
  :bind
  (:map dot-mode-map
        ("C-." . #'+meow-dot-mode-execute)))

;;;###autoload
(defun +meow-dot-mode-execute ()
  (interactive)
  (with-undo-amalgamate
    (meow-insert)
    (call-interactively #'dot-mode-execute)
    (meow-insert-exit)))

(use-package helpful ; better help menu
  :defer 0.7
  :bind
  (:map +leader-map
        ("hk" . #'helpful-key))
  (:map meow-normal-state-keymap
        ([remap describe-command] . helpful-command)
        ([remap describe-key] . helpful-key)
        ([remap describe-variable] . helpful-variable)
        ([remap describe-function] . helpful-function)
        ([remap describe-symbol] . helpful-symbol)))

(when IS-LINUX
  (add-hook 'after-init-hook
            (lambda ()
              (setq exec-path (append exec-path '("~/go/bin"))))))

(when (version< emacs-version "29.1")
  (use-package exec-path-from-shell ; Use system $PATH variable for eshell, commands, etc.
    :custom
    (exec-path-from-shell-arguments '("-l"))
    (sh-shell-file "/usr/bin/zsh")
    (shell-file-name "zsh")
    :config
    (add-hook 'after-init-hook
              (lambda ()
                (setq exec-path-from-shell-arguments '("-l"))
                (exec-path-from-shell-copy-env "PYTHONPATH")
                (exec-path-from-shell-initialize)
                (setq exec-path (append exec-path '("/home/labergeron/miniconda3/bin" "~/go/bin")))))))

(use-package google-this
  :after embark
  :commands (google-this-symbol)
  :bind
  (:map embark-general-map
        ("g" . google-this-word)))

(use-package ace-link
  :bind
  (:map text-mode-map
        ("M-i" . #'ace-link))
  (:map helpful-mode-map
        ("M-i" . #'ace-link)))

(use-package evil-matchit
  :bind
  (:map meow-normal-state-keymap
        ("%" . #'evilmi-jump-items-native)))

(use-package beginend
  :disabled t
  :bind
  (:map +leader-map
        ("[" . #'beginning-of-buffer)
        ("]" . #'end-of-buffer))
  :hook (after-init . beginend-global-mode))

;; buffers restored from desktop.el initialize in fundamental-mode so this sets it correctly
(defun +restore-major-mode ()
  (when (and (eq major-mode 'fundamental-mode)
             (buffer-file-name))
    (set-auto-mode)))

(add-hook 'window-configuration-change-hook '+restore-major-mode)
