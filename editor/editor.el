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
  :bind
  (:map meow-normal-state-keymap
        ("C-p" . goto-last-change)
        ("C-n" . goto-last-change-reverse)))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  (embark-confirm-act-all nil)
  :bind
  (:map minibuffer-mode-map
        ("M-." . #'embark-act)
        ("M-," . #'+embark-select)
        ("M-a" . #'embark-act-all)
        ("M-r" . #'embark-become)
        ("C-c C-e" . #'embark-export))
  (:map embark-symbol-map
        ("h" . #'helpful-symbol))
  (:map meow-motion-mode-hook
        ("M-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-become-help-map
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("k" . #'helpful-key))
  (:map meow-normal-state-keymap
        ("M-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-general-map
        ("'" . #'embark-dwim)
        ("f" . #'consult-ripgrep)
        ("d" . #'embark-find-definition)
        ("g" . #'google-this-word))
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

(use-package undo-tree ; Persistent Undos
  :defer 0.1
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :custom
  (undo-limit 10000)
  (undo-tree-auto-save-history t)
  :bind
  (:map meow-normal-state-keymap
        ("u" . #'undo-tree-undo)
        ("U" . #'undo-tree-redo))
  (:map +leader-map
        ("fu" . #'undo-tree-visualize))
  :config
  (global-undo-tree-mode))

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

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("R" . #'+grep-wgrep-replace)
        ("i" . #'wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map
        ([remap save-buffer] . +wgrep-finish-edit)
        ("R" . #'+wgrep-replace)))

;;;###autoload
(defun +grep-wgrep-replace ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (call-interactively #'+wgrep-replace))

;;;###autoload
(defun +wgrep-finish-edit ()
  (interactive)
  (wgrep-finish-edit)
  (save-some-buffers t))

;;;###autoload
(defun +wgrep-replace (regexp replace)
  "Replace in wgrep without replacing the read-only 'file_name:line:' prefix."
  (interactive (list (read-string "Replace: ")
                     (read-string "Replace With: ")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([^:]*:[0-9]+:\\)" nil t)
      (let ((prefix-end (point))
            (line-end (line-end-position)))
        (while (re-search-forward regexp (line-end-position) t)
          (replace-match replace t nil))
        (forward-line)
        (beginning-of-line)))))

(use-package evil-matchit
  :bind
  (:map meow-normal-state-keymap
        ("%" . #'evilmi-jump-items-native)))
