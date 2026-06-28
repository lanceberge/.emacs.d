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
     (+find-file-new-window . nil)
     (+switch-to-buffer-new-window . nil)
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
        ("M-e" . #'embark-export))
  (:map embark-symbol-map
        ("s" . #'+project-replace-regex)
        ("h" . #'helpful-symbol))
  (:map +motion-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-become-file+buffer-map
        ("k" . #'+project-switch-project)
        ("'" . #'project-find-file)
        (";" . #'consult-ripgrep)
        ("," . #'+consult-project-buffer)
        ("d" . #'consult-dir))
  (:map embark-become-help-map
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("k" . #'helpful-key))
  (:map embark-region-map
        ("s" . #'+project-replace-regex))
  (:map embark-buffer-map
        ("e" . #'project-eshell)
        ("o" . #'+ace-window-switch-to-buffer)
        ("N" . #'+switch-to-buffer-new-window))
  (:map embark-file-map
        ("e" . #'project-eshell)
        ("o" . #'+ace-window-find-file)
        ("N" . #'+find-file-new-window))
  (:map embark-general-map
        (";" . #'consult-ripgrep)
        ("d" . #'embark-find-definition)
        ("/" . #'consult-line)
        ("g" . #'goolge-this-word))
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

  (advice-add 'embark-act :before #'force-keycast-update))

(use-package embark-this-buffer
  :ensure (:type file :main "~/.emacs.d/lisp/embark-this-buffer.el")
  :bind
  (:map ctl-x-map
        ("rb" . #'+bookmark-file))
  (:map +leader-map
        ("." . #'embark-on-this-buffer)))

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
  (:map +leader-map
        ("hk" . #'helpful-key))
  (:map +normal-mode-map
        ([remap describe-command] . helpful-command)
        ([remap describe-key] . helpful-key)
        ([remap describe-variable] . helpful-variable)
        ([remap describe-function] . helpful-function)
        ([remap describe-symbol] . helpful-symbol)))

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

(use-package ace-link
  :bind
  (:map text-mode-map
        ("M-i" . #'ace-link))
  (:map helpful-mode-map
        ("M-i" . #'ace-link)))

(use-package beginend
  :disabled t
  :bind
  (:map +leader-map
        ("[" . #'beginning-of-buffer)
        ("]" . #'end-of-buffer))
  :hook (after-init . beginend-global-mode))

;; TODO idk why i thought i needed this
;; buffers restored from desktop.el initialize in fundamental-mode so this sets it correctly
;; (add-hook 'window-configuration-change-hook '+restore-major-mode)

;;;###autoload
(defun +restore-major-mode ()
  (when (and (eq major-mode 'fundamental-mode)
             (buffer-file-name))
    (set-auto-mode)))

(use-package jinx
  ;; :disabled t
  :hook ((prog-mode text-mode) . jinx-mode)
  :bind
  (:map jinx-mode-map
        ("M-=" . #'jinx-correct)))

(use-package +surround
  :ensure (:type file :main "~/.emacs.d/lisp/surround.el")
  :hook
  (org-mode . +setup-org-pairs)
  :bind
  (:map +normal-mode-map
        ("S" . #'+surround)))

(use-package +toggle-case
  :ensure (:type file :main "~/.emacs.d/lisp/toggle-case.el")
  :bind
  (:map +normal-mode-map
        ("~" . #'+toggle-region-or-number-dwim))
  :config
  (put 'upcase-region 'disabled nil))

(use-package +text-extras
  :ensure (:type file :main "~/.emacs.d/lisp/text-extras.el")
  :bind
  (:map embark-region-map
        ("|" . #'pipe-region))

  (:map +leader-map
        ("u" . #'text-to-clipboard)))

(use-package +increment-number
  :ensure (:type file :main "~/.emacs.d/lisp/increment-number.el")
  :bind
  (:map +normal-mode-map
        ("M-`" . #'+increment-number-increment)
        ("M-~" . #'+increment-number-decrement)))

(use-package +mark-forward-backward
  :ensure (:type file :main "~/.emacs.d/lisp/mark-forward-backward.el")
  :bind
  (:repeat-map mark-backward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-backward-sentence )
               ("p" . #'+mark-backward-paragraph)
               ("w" . #'+mark-backward-word)
               ("d" . #'+mark-backward-sexp)
               :exit
               ("g" . (lambda () (interactive))))
  (:map +normal-mode-map
        ("B" . #'+mark-backward-word))

  (:map mark-backward-keymap
        ("p" . #'+mark-backward-paragraph)
        ("d" . #'+mark-backward-sexp)
        ("s" . #'+mark-backward-sentence)
        ("w" . #'+mark-backward-word)
        ("b" . #'+mark-backward-buffer)))

(use-package +mark-backward
  :ensure nil
  :bind
  (:repeat-map mark-forward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-forward-sentence )
               ("p" . #'+mark-forward-paragraph)
               ("w" . #'+mark-forward-word)
               ("d" . #'+mark-forward-sexp)
               :exit
               ("g" . (lambda () (interactive))))
  (:map mark-forward-keymap
        ("p" . #'+mark-forward-paragraph)
        ("w" . #'+mark-forward-word)
        ("d" . #'+mark-forward-sexp)
        ("s" . #'+mark-forward-sentence)
        ("b" . #'+mark-forward-buffer)))

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

(use-package +dot-repeat
  :ensure (:type file :main "~/.emacs.d/lisp/dot-repeat/dot-repeat.el")
  :hook (after-init . +dot-repeat-mode)
  :bind
  (:map +normal-mode-map
        ("." . #'+dot-repeat)))

(use-package narrow-extras
  :ensure (:type file :main "~/.emacs.d/lisp/narrow-extras.el")
  :bind
  (:map ctl-x-map
        ;; ("nn" . #'narrow-or-widen-dwim)
        ))

(use-package puni)

(use-package puni-extensions
  :ensure (:type file :main "~/.emacs.d/lisp/puni-extensions.el")
  :bind
  (:map +insert-mode-map
        ("C-(" . #'+puni-slurp-or-barf-left)
        ("C-)" . #'+puni-slurp-or-barf-right))
  (:map +normal-mode-map
        ("(" . +puni-slurp-or-barf-left)
        (")" . +puni-slurp-or-barf-right)))

(use-package visiting-buffer
  :ensure (:type file :main "~/.emacs.d/lisp/visiting-buffer.el")
  :demand t)
