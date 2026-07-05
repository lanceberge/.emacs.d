;;; -*- lexical-binding: t -*-
(use-package apheleia ;; format on save
  :hook
  ((emacs-lisp-mode bash-ts-mode) . apheleia-mode))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  (embark-confirm-act-all nil)
  (embark-quit-after-action
   '(
     ;; (+window-find-file-new . nil)
     ;; (+window-find-file-new-largest . nil)
     ;; (+window-find-file-new-largest-action . nil)
     ;; (+window-switch-to-buffer-new . nil)
     ;; (+window-switch-to-buffer-new-action . nil)
     (t . t)))
  :bind
  (:map +normal-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map +insert-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map minibuffer-mode-map
        ("C-." . #'embark-act)
        ("M-," . #'+embark-select)
        ("M-a" . #'embark-act-all)
        ("M-m" . #'embark-become)
        ("M-e" . #'embark-export)
        ("C-c C-c" . #'embark-collect))
  (:map embark-symbol-map
        ("h" . #'helpful-symbol))
  (:map +motion-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-become-file+buffer-map
        ("k" . #'+project-switch-project)
        ("f" . #'project-find-file)
        ("d" . #'project-find-dir)
        ("g" . #'consult-ripgrep)
        ("b" . #'+consult-project-buffer))
  (:map embark-become-help-map
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("k" . #'helpful-key))
  (:map embark-buffer-map
        ("e" . #'project-eshell)
        ("o" . #'+window-ace-switch-to-buffer)
        ("N" . #'+window-switch-to-buffer-new-action))
  (:map embark-file-map
        ("e" . #'project-eshell)
        ("o" . #'+window-ace-find-file)
        ("N" . #'+window-find-file-new-largest-action))
  (:map embark-collect-mode-map
        ("F" . #'consult-focus-lines))
  (:map embark-identifier-map
        ("R" . #'eglot-rename)
        ("SPC" . #'eglot-code-actions))
  (:map help-map
        ("b" . #'embark-bindings))
  :config
  (add-to-list 'embark-around-action-hooks
               '(pipe-region embark--mark-target))
  (add-to-list 'embark-target-injection-hooks
               '(pipe-region embark--ignore-target))

  ;; Noconform embark actions
  (setq embark-pre-action-hooks
        (cl-remove-if (lambda (hook)
                        (eq (car (cdr hook)) 'embark--confirm))
                      embark-pre-action-hooks))

  ;; support showing embark actions in the keycast modeline
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#showing-embark-actions-keys-in-keycast-mode
  (defun store-action-key+cmd (cmd)
    (force-mode-line-update t)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command-desc cmd))

  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)

  ;; version of keycast--update that accepts (and ignores) parameters
  (defun force-keycast-update (&rest _) (keycast--update))

  (advice-add 'embark-act :before #'force-keycast-update)

  (defvar-keymap +embark-priority-map
    :doc "Embark bindings that take precedence over target-specific maps."
    "s l" #'consult-line
    "s g" #'consult-ripgrep)

  (advice-remove #'embark--action-keymap #'+embark-apply-priority-map)
  (advice-add #'embark--action-keymap :filter-return #'+embark-apply-priority-map))

;;;###autoload
(defun +embark-apply-priority-map (keymap)
  "Compose `+embark-priority-map' before the Embark action KEYMAP."
  (make-composed-keymap +embark-priority-map keymap))

(use-package this-buffer
  :ensure (:type file :main "~/.emacs.d/lisp/this-buffer.el" :files ("this-buffer.el"))
  :bind
  (:map ctl-x-map
        ("rb" . #'+bookmark-file))
  (:map +leader-map
        (".!" . #'shell-command)
        (".$" . #'ispell)
        (".&" . #'async-shell-command)
        (".=" . #'apheleia-format-buffer)
        (". C-=" . #'ediff-buffers)
        (".N" . #'+this-buffer-open-in-new-window)
        (". RET" . #'eval-buffer)
        (".W" . #'write-file)
        (".b" . #'+bookmark-file)
        (".c" . #'copy-file)
        (".d" . #'delete-file)
        (".e" . #'eval-buffer)
        (".f" . #'consult-focus-lines)
        (".g" . #'+window-revert-buffer)
        (".h" . #'mark-whole-buffer)
        (".k" . #'consult-keep-lines)
        (".l" . #'load-file)
        (".n" . #'+this-buffer-diff-with-file)
        (".o" . #'+this-buffer-move-to-window)
        (".p" . #'pwd)
        (".q" . #'quit-window)
        (".r" . #'rename-file)
        (".v d" . #'vc-delete-file)
        (".v r" . #'vc-rename-file)
        (".w" . #'+file-name-kill-ring-save)
        (".x" . #'+this-buffer-open-externally)
        (".z" . #'bury-buffer)
        (".|" . #'+this-buffer-shell-command-on-buffer)))

;;;###autoload
(defun +embark-select ()
  "Select a minibuffer candidate and go to the next one."
  (interactive)
  (let ((vertico-cycle nil))
    (embark-select)
    (vertico-next)))

(use-package helpful ; better help menu
  :defer 0.7
  :bind
  (:map help-map
        ("x" . helpful-command)
        ("C-h")
        ("k" . #'helpful-key)
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("s" . #'helpful-symbol)))

(add-hook 'after-init-hook
          (lambda ()
            (add-to-list 'exec-path "~/go/bin")
            (add-to-list 'exec-path "~/.cargo/bin")))

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

;; buffers restored from desktop.el initialize in fundamental-mode so this sets it correctly
(add-hook 'window-configuration-change-hook '+restore-major-mode)

;;;###autoload
(defun +restore-major-mode ()
  (when (and (eq major-mode 'fundamental-mode)
             (buffer-file-name))
    (set-auto-mode)))

(use-package jinx
  :hook ((prog-mode text-mode) . jinx-mode)
  :bind
  (:map jinx-mode-map
        ("M-=" . #'jinx-correct)))

(use-package +toggle-case
  :ensure (:type file :main "~/.emacs.d/lisp/toggle-case.el" :files ("toggle-case.el"))
  :bind
  (:map +normal-mode-map
        ("~" . #'+toggle-region-or-number-dwim))
  :config
  (put 'upcase-region 'disabled nil))

(use-package +text-extras
  :ensure (:type file :main "~/.emacs.d/lisp/text-extras.el" :files ("text-extras.el"))
  :bind
  (:map embark-region-map
        ("|" . #'pipe-region))

  (:map +leader-map
        ("u" . #'text-to-clipboard)))

(use-package increment-number
  :ensure (:type file :main "~/.emacs.d/lisp/increment-number.el" :files ("increment-number.el"))
  :bind
  (:map +normal-mode-map
        ("M-`" . #'+increment-number-increment)
        ("M-~" . #'+increment-number-decrement)))

(use-package newcomment
  :ensure nil
  :bind
  (:map text-mode-map
        ("M-;" . #'+comment-dwim))
  (:map prog-mode-map
        ("M-;" . #'+comment-dwim)))

;;;###autoload
(defun +comment-dwim ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'comment-dwim)
    (call-interactively #'comment-line)))

(use-package easy-kill
  :custom
  (easy-kill-alist
   `((?f word " ")
     (?s sexp "\n")
     (?d defun "\n\n")
     (?D defun-name " ")
     (?l line "\n")))
  :bind
  (:map easy-kill-base-map
        ("SPC" . #'easy-kill-mark-region)
        ("w" . #'easy-kill-region)
        ([remap +modal-kill-region-insert] . #'+easy-kill-region-insert))
  (:map +normal-mode-map
        ("," . #'easy-mark)
        ([remap kill-ring-save] . #'easy-kill)))

;;;###autoload
(defun +easy-kill-region-insert ()
  (interactive)
  (call-interactively #'easy-kill-region)
  (+insert-mode))

(use-package narrow-extras
  :ensure (:type file :main "~/.emacs.d/lisp/narrow-extras.el" :files ("narrow-extras.el"))
  :bind
  (:map ctl-x-map
        ("n" . #'narrow-or-widen-dwim)))

(defvar +puni-mode-map (make-sparse-keymap)
  "Keymap for `+puni-mode'.")

;;;###autoload
(define-minor-mode +puni-mode
  "Use puni commands for editing operations."
  :lighter " Puni"
  :keymap +puni-mode-map
  (setq-local +puni-extras-default-kill-region-command #'+puni-soft-kill-region))

(use-package puni
  :hook (prog-mode . +puni-mode)
  :init
  (+modal-create-insert-function puni-kill-line)
  (+modal-create-insert-function puni-forward-delete-char)
  (+modal-create-insert-function puni-kill-region)
  (+modal-create-insert-function puni-forward-kill-word)
  (+modal-create-insert-function puni-backward-kill-word)
  (+modal-create-insert-function puni-backward-kill-line)
  :bind
  (:map +normal-mode-map
        ("\\f" . #'puni-forward-sexp)
        ("\\b" . #'puni-backward-sexp)
        ("\\T" . #'puni-convolute)
        ("\\r" . #'puni-raise)
        ("S" . #'puni-squeeze)
        ("K" . #'puni-backward-kill-line)
        ("C-S-k" . #'+modal-puni-backward-kill-line-insert))
  (:map +puni-mode-map
        ([remap mark-sexp] . #'puni-mark-sexp-at-point)
        ([remap +modal-kill-line-insert] . #'+modal-puni-kill-line-insert)
        ([remap +modal-delete-char-insert] . #'+modal-puni-forward-delete-char-insert)
        ([remap +modal-kill-word-insert] . #'+modal-puni-forward-kill-word-insert)
        ([remap +modal-backward-kill-word-insert] . #'+modal-puni-backward-kill-word-insert)
        ([remap kill-line] . #'puni-kill-line)
        ([remap delete-char] . #'puni-forward-delete-char)
        ([remap kill-region] . #'puni-kill-region)
        ([remap kill-word] . #'puni-forward-kill-word)
        ([remap backward-kill-word] . #'puni-backward-kill-word)
        ([remap backward-delete-char-untabify] . #'puni-backward-delete-char)))

(use-package puni-repeat
  :ensure nil
  :bind
  (:repeat-map +puni-repeat-map
               ("." . #'repeat))
  (:map +normal-mode-map
        ("\\l" . #'puni-slurp-forward)
        ("\\h" . #'puni-slurp-backward)
        ("\\H" . #'puni-barf-forward)
        ("\\L" . #'puni-barf-backward))
  :config
  (require 'puni))

(use-package puni-extras
  :ensure (:type file :main "~/.emacs.d/lisp/puni-extras.el" :files ("puni-extras.el"))
  :init
  (+modal-create-insert-function +puni-kill-region-or-inner-sexp)
  (+modal-create-insert-function +puni-kill-region-or-outer-sexp)
  (+modal-create-insert-function +puni-soft-kill-region)
  :bind
  (:map +normal-mode-map
        ("C-w" . #'+modal-puni-kill-region-or-inner-sexp-insert)
        ("C-W" . #'+modal-puni-kill-region-or-outer-sexp-insert)
        ("w" . #'+puni-kill-region-or-inner-sexp)
        ("W" . #'+puni-kill-region-or-outer-sexp))
  (:map +puni-mode-map
        ([remap +kill-line-dwim] . #'+puni-kill-line-dwim)
        ([remap +kill-line-dwim] . #'+puni-kill-line-dwim)))

(use-package visiting-buffer
  :ensure (:type file :main "~/.emacs.d/lisp/visiting-buffer.el" :files ("visiting-buffer.el"))
  :demand t)

(use-package mark-forward-backward
  :ensure (:type file :main "~/.emacs.d/lisp/mark-forward-backward.el" :files ("mark-forward-backward.el"))
  :init
  (dolist (function '(+mark-forward-char
                      +mark-backward-char
                      +mark-forward-line
                      +mark-forward-word
                      +mark-backward-word))
    (eval `(+modal-create-insert-function ,function)))
  :bind
  (:map +insert-mode-map
        ("M-F" . #'+mark-forward-word)
        ("M-B" . #'+mark-backward-word)
        ("C-S-p" . #'+mark-backward-line)
        ("C-S-f" . #'+mark-forward-char)
        ("C-S-b" . #'+mark-backward-char)
        ("C-S-n" . #'+mark-forward-line))
  (:map +normal-mode-map
        ("C-S-f" . #'+modal-mark-forward-char-insert)
        ("C-S-b" . #'+modal-mark-backward-char-insert)
        ("C-S-n" . #'+modal-mark-forward-line-insert)
        ("C-S-p" . #'+modal-mark-backward-line-insert)
        ("M-F" . #'+modal-mark-forward-word-insert)
        ("M-B" . #'+modal-mark-backward-word-insert)
        ("F" . #'+mark-forward-word)
        ("B" . #'+mark-backward-word)
        ("N" . #'+mark-forward-line)
        ("P" . #'+mark-backward-line)))

(use-package editor-lisp
  :ensure (:type file :main "~/.emacs.d/lisp/editor-lisp.el" :files ("editor-lisp.el"))
  :demand t)

;;;###autoload
(defun +kill-whitespace-ignore (orig-fn string &rest args)
  "https://stackoverflow.com/questions/12102554/emacs-skip-whitespace-kills"
  (let* (
         (string-raw (substring-no-properties string))
         (space-p (not (string-match-p "[^ \t\n\r]" string-raw))))

    (cond
     ((not space-p)
      (apply orig-fn string args))
     (t
      (message "skipped whitespace kill")))))

(advice-add 'kill-new :around #'+kill-whitespace-ignore)
