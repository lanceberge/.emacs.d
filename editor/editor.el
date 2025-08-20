;;; -*- lexical-binding: t -*-
(use-package format-all ; format code functions
  :hook
  ((prog-mode
    web-mode
    json-mode
    svelte-mode) . format-all-mode)
  :custom
  (format-all-show-errors t)
  :general
  (my-leader-def
    :states 'normal
    "=" #'(+format/buffer :which-key "format"))
  ;; :general
  ;; TODO
  ;; ('meow-normal-state-keymap
  ;;  "[of" (defun +format-all-off () (interactive)
  ;;               (format-all-mode -1) :which-key "format-all")
  ;;  "]of" (defun +format-all-on  () (interactive)
  ;;               (format-all-mode 1) :which-key "format all"))
  :config
  (setq-default format-all-formatters format-all-default-formatters))

(use-package avy
  :custom
  (avy-keys '(?j ?d ?k ?s ?l ?a))
  :general
  ('(normal insert)
   "M-i" #'avy-pop-mark)

  ;; ('meow-normal-state-keymap
  ;;  "s" #'(+avy-goto-char-2-below :which-key "2-chars")
  ;;  "S" #'(+avy-goto-char-2-above :which-key "2-chars"))
  :config
  (setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-char-2-below . avy-order-closest)
                           (avy-goto-char-2-above . avy-order-closest)))
  ;; https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-embark (pt)
    "Perform an embark action on the avy target without moving point to it"
    (unwind-protect
        (save-excursion
          (avy-action-embark-move pt))
      (select-window
       (cdr (ring-ref avy-ring 0)))) t)

  (defun avy-action-embark-move (pt)
    "Perform an embark action on the avy target and move the point to it"
    (goto-char pt)
    (embark-act))

  (setq avy-dispatch-alist
        (list
         (cons ?\s 'avy-action-embark-move)
         (cons ?, 'avy-action-embark))))

(use-package embark
  :general
  ('(meow-insert-state-keymap meow-normal-state-keymap meow-motion-state-keymap global-map minibuffer-mode-map)
   "M-." #'embark-act
   "M-," #'embark-export)
  ('embark-general-map
   "$" nil
   ";" #'flyspell-auto-correct-word
   "y" #'define-word-at-point
   "d" #'embark-find-definition
   "g" #'google-this-word)
  ('embark-identifier-map
   "." #'lsp-execute-code-action)
  :config
  ;; Noconform actions embark
  (setq embark-pre-action-hooks
        (cl-remove-if (lambda (hook)
                        (eq (car (cdr hook)) 'embark--confirm))
                      embark-pre-action-hooks)))

(use-package define-word
  :commands (define-word-at-point))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer)
  (dired-recursive-copies 'always)
  :general
  (my-leader-def
    "-" #'(dired-jump :which-key "open dired"))
  ('meow-normal-state-keymap 'dired-mode-map
                             "i" #'+dired/edit)
  ('dired-mode-map
   "i" #'dired-toggle-read-only)
  ('wdired-mode-map
   [remap save-buffer] #'wdired-finish-edit)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired

(use-package helpful ; better help menu
  :defer 0.7
  :general
  (my-leader-def
    "hk" #'helpful-key)

  ('meow-normal-state-keymap helpful-mode-map
                             "q" #'quit-window)

  ([remap describe-command] #'helpful-command
   [remap describe-key] #'helpful-key
   [remap describe-variable] #'helpful-variable
   [remap describe-function] #'helpful-function
   [remap describe-symbol] #'helpful-symbol))

(use-package undo-tree ; Persistent Undos
  :defer 0.1
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :custom
  (undo-limit 10000)
  (undo-tree-auto-save-history t)
  :general
  ('meow-normal-state-keymap
   "u" (defun +undo () (interactive)
              (if (region-active-p)
                  (undo 1)
                (undo-tree-undo)))
   "C-r" (defun +redo () (interactive)
                (if (region-active-p)
                    (redo 1)
                  (undo-tree-redo))))

  (my-leader-def
    "fu" #'(undo-tree-visualize :which-key "undo"))
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
  :commands (google-this-symbol))

(use-package ace-link
  :general
  ('(org-mode-map helpful-mode-map)
   "M-i" #'(ace-link :which-key "goto link")))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  :general
  ('meow-normal-state-keymap 'grep-mode-map
                             "R" (defun +wgrep-edit-and-replace () (interactive)
                                        (wgrep-change-to-wgrep-mode)
                                        (call-interactively #'+wgrep-replace))
                             "i" #'wgrep-change-to-wgrep-mode)
  ('meow-normal-state-keymap wgrep-mode-map
                             "R" #'+wgrep-replace)
  :defer t)

(use-package restart-emacs
  :general
  (my-leader-def
    "re" #'(+restart-emacs :which-key "restart emacs"))
  :config
  (defun +restart-emacs ()
    (interactive)
    (let ((vterm- (get-buffer "*vterm*")))
      (if vterm-
          (kill-buffer )))
    (setq confirm-kill-emacs nil)
    (restart-emacs)))

(use-package drag-stuff
  :general
  ('meow-normal-state-keymap
   "M-k" #'drag-stuff-up
   "M-j" #'drag-stuff-down))

(use-package expand-region
  :general
  ('(meow-normal-state-keymap meow-motion-state-keymap)
   "o" #'er/expand-region)
  :config
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs)))

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
  "Replace in wgrep without replacing the read only 'file_name:line:' prefix."
  (interactive (list  (read-string "Replace: ")
                      (read-string "Replace With: ")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([^:]*:[0-9]+:\\)" nil t)
      (let ((line-end ( line-end-position)))
        (while (re-search-forward regexp line-end t)
          (replace-match replace t nil))
        (forward-line 1)
        (beginning-of-line)))))
