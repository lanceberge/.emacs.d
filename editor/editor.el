;;; -*- lexical-binding: t -*-
(use-package apheleia ;; format on save
  :hook
  ((emacs-lisp-mode bash-ts-mode) . apheleia-mode)
  :bind
  (:map +leader-map
        ("=" . #'apheleia-format-buffer)))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  (embark-confirm-act-all nil)
  (embark-quit-after-action
   '((find-file . t)
     (consult-grep . t)
     (consult-ripgrep . t)
     (eshell . t)
     (project-eshell . t)
     (describe-symbol . nil)
     (embark-copy-as-kill . nil)
     (t . nil)))
  :bind
  (:map +normal-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map minibuffer-mode-map
        ("C-." . #'embark-act)
        ("M-," . #'+embark-select)
        ("M-a" . #'embark-act-all)
        ("M-r" . #'embark-become)
        ("M-e" . #'embark-export))
  (:map embark-symbol-map
        ("s" . #'+project-replace-regex)
        ("h" . #'helpful-symbol))
  (:map +motion-mode-map
        ("C-." . #'embark-act)
        ("M-'" . #'embark-dwim))
  (:map embark-become-help-map
        ("v" . #'helpful-variable)
        ("f" . #'helpful-function)
        ("k" . #'helpful-key))
  (:map embark-region-map
        ("s" . #'+project-replace-regex))
  (:map embark-file-map
        ("y" . #'project-eshell))
  (:map embark-general-map
        ("'" . #'embark-dwim)
        (";" . #'consult-ripgrep)
        ("d" . #'embark-find-definition)
        ("/" . #'consult-line)
        ("W" . #'+file-name-kill-ring-save)
        ("g" . #'goolge-this-word))
  (:map embark-collect-mode-map
        ("F" . #'consult-focus-lines))
  (:map embark-identifier-map
        ("SPC" . #'eglot-code-actions))
  :config
  ;; Noconform embark actions
  (setq embark-pre-action-hooks
        (cl-remove-if (lambda (hook)
                        (eq (car (cdr hook)) 'embark--confirm))
                      embark-pre-action-hooks)))

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

;; buffers restored from desktop.el initialize in fundamental-mode so this sets it correctly
;;;###autoload
(defun +restore-major-mode ()
  (when (and (eq major-mode 'fundamental-mode)
             (buffer-file-name))
    (set-auto-mode)))

(use-package harpoon
  :ensure (:type file :main "~/.emacs.d/packages/harpoon.el")
  :bind
  (:map +leader-map
        ("fh" . #'+consult-harpoon-bookmarks))
  :init
  (dotimes (i 10)
    (let ((num (number-to-string i)))
      (define-key +leader-map num
                  `(lambda ()
                     (interactive)
                     (+harpoon-goto ,num)))
      (define-key +leader-map (kbd (format "SPC %s" num))
                  `(lambda ()
                     (interactive)
                     (+harpoon-bookmark ,num))))))

(use-package jinx
  ;; :disabled t
  :hook ((prog-mode text-mode) . jinx-mode)
  :bind
  (:map +leader-map
        ("c=" . #'jinx-correct-all))
  (:map jinx-mode-map
        ("M-=" . #'jinx-correct)))

(use-package +surround
  :ensure (:type file :main "~/.emacs.d/packages/surround.el")
  :hook
  (org-mode . +setup-org-pairs)
  :bind
  ("M-s M-s" . #'+surround)
  (:map +normal-mode-map
        ("S" . #'+surround)))

(use-package +toggle-case
  :ensure (:type file :main "~/.emacs.d/packages/toggle-case.el")
  :bind
  (:map +normal-mode-map
        ("~" . #'+toggle-region-or-number-dwim))
  :config
  (put 'upcase-region 'disabled nil))

(use-package +text-extras
  :ensure (:type file :main "~/.emacs.d/packages/text-extras.el")
  :bind
  (:map +normal-mode-map
        ("|" . #'pipe-region))

  (:map +leader-map
        ("u" . #'text-to-clipboard)
        ("bw" . #'+file-name-kill-ring-save)))

(use-package +increment-number
  :ensure (:type file :main "~/.emacs.d/packages/increment-number.el")
  :bind
  (:map +normal-mode-map
        ("M-`" . #'+increment-number-increment)
        ("M-~" . #'+increment-number-decrement)))

(use-package +mark-forward-backward
  :ensure (:type file :main "~/.emacs.d/packages/mark-forward-backward.el")
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

(use-package mark-repeat-map
  :ensure nil
  :bind
  (:repeat-map +mark-repeat-map
               ("[" . #'+backward-global-mark)
               ("]" . #'+forward-global-mark))
  (:map +leader3-map
        ("[" . #'+backward-global-mark)
        ("]" . #'+forward-global-mark)))

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

(add-hook 'window-configuration-change-hook '+restore-major-mode)
