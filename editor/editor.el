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

(use-package avy
  :custom
  (avy-keys '(?j ?d  ?s ?l ?a))
  (avy-single-candidate-jump nil)
  :bind
  (:map meow-normal-state-keymap
        ("F" . #'avy-goto-char-2))
  :config
  (setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-char-2-below . avy-order-closest)
                           (avy-goto-char-2-above . avy-order-closest)))
  ;; https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-embark (pt)
    "Perform an embark action on the avy target and move the point to it"
    (goto-char pt)
    (embark-act))

  (setq avy-dispatch-alist
        (list
         (cons ?m 'avy-action-mark-until-pt)
         (cons ?k 'avy-action-kill-lines-to-point-stay)
         (cons ?, 'avy-action-embark)
         (cons ?K 'avy-action-kill-line-stay)
         (cons ?t 'avy-action-move-region))))

;;;###autoload
(defun avy-action-kill-line-stay (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line)))

;;;###autoload
(defun avy-action-kill-lines-to-point-stay (pt)
  "Kill from current point to the match point PT, excluding the match."
  (save-excursion
    (kill-region (point) (progn (goto-char pt) (end-of-visual-line) (point)))))

;;;###autoload
(defun avy-action-mark-until-pt (pt)
  (if (region-active-p)
      (goto-char pt)
    (progn
      (set-mark (point))
      (goto-char pt))))

;;;###autoload
(defun avy-action-move-region (pt)
  (let* ((region-active (region-active-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-beginning-position 2))))
    (kill-region beg end)
    (goto-char pt)
    (unless region-active
      (beginning-of-visual-line))
    (yank)))

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
        ("C-r" . #'undo-tree-redo))
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
        ("R" .  (lambda () (interactive)
                  (wgrep-change-to-wgrep-mode)
                  (call-interactively #'+wgrep-replace)))
        ("i" . wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map
        ([remap save-buffer] . wgrep-finish-edit)
        ("R" . +wgrep-replace)))

(use-package expand-region
  :bind
  (:map meow-normal-state-keymap
        ("o" . #'er/expand-region)
        ("O" . #'+expand-region-2))
  (:map meow-motion-state-keymap
        ("o" . #'expand-region)
        ("O" . +expand-region-2))
  :config
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs)))

(defun +expand-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (let ((point-at-beginning (eq (point) (region-beginning))))
        (unless point-at-beginning
          (exchange-point-and-mark))
        (er/expand-region arg))
    (er/expand-region arg)))

;;;###autoload
(defun +expand-region-2 ()
  (interactive)
  (+expand-region 2))

;;;###autoload
(defun +avy-goto-char-2-below (char1 char2)
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)))
  (forward-char 1)
  (search-forward  (concat (char-to-string char1) (char-to-string char2)))
  (backward-char 2)
  (avy-goto-char-2
   char1 char2 nil
   (point) (window-end (selected-window) t)))

;;;###autoload
(defun +avy-goto-char-2-above (char1 char2)
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)))
  (backward-char 1)
  (search-backward (concat (char-to-string char1) (char-to-string char2)))
  (forward-char 2)
  (avy-goto-char-2-above
   char1 char2))

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
