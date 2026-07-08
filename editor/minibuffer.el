;;; -*- lexical-binding: t -*-
(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . (lambda () (+insert-mode 1)))
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t
                                            face minibuffer-prompt))
  :bind
  (:map minibuffer-inactive-mode-map
        ([remap save-buffer] . #'kill-current-buffer))
  (:map minibuffer-mode-map
        ([remap newline] . #'exit-minibuffer)
        ([remap minibuffer-keyboard-quit] . #'abort-minibuffers)
        ([remap +keyboard-quit-normal] . #'abort-minibuffers)
        ("C-g" . #'abort-minibuffers)))

(use-package consult
  :defer 1.5
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "C->")
  (consult-project-buffer-sources '(consult-source-project-buffer
                                    consult-source-project-recent-file))
  :bind
  (:map goto-map
        ("g" . #'consult-goto-line)
        ("M-g" . #'consult-goto-line)
        ("i" . #'consult-imenu)
        ("M-i" . #'consult-imenu-multi)
        ("e" . #'consult-flymake)
        ("m" . #'consult-mark)
        ("M-m" . #'consult-global-mark))
  (:map ctl-x-map
        ("b" . #'consult-buffer)
        ("rb" . #'consult-bookmark)
        ("rj" . #'consult-register-load)
        ("rs" . #'consult-register-store)
        ("M-x" . #'consult-mode-command)
        (":" . #'consult-complex-command)
        ;; ("'" . #'consult-recent-file)
        )
  (:map minibuffer-mode-map
        ("M-r" . #'consult-history))
  (:map isearch-mode-map
        ("M-r" . #'consult-isearch-history))
  (:map project-prefix-map
        ("g" . #'consult-ripgrep)
        ("i" . #'consult-imenu-multi)
        ("l" . #'consult-line-multi))
  (:map help-map
        ("C-m" . #'consult-minor-mode-menu))
  (:map search-map
        ("g" . #'consult-ripgrep)
        ("r" . #'consult-recent-file)
        ("l" . #'consult-line)
        ("M-l" . #'consult-line-multi))
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))

(use-package consult-extras
  :ensure (:type file :main "~/.emacs.d/lisp/consult-extras.el" :files ("consult-extras.el"))
  :custom
  (consult-preview-excluded-buffers #'+consult-preview-tramp-excluded-p)
  :bind
  (:map +normal-mode-map
        ("M-q" . #'+consult-kmacro)
        ("M-y" . #'+consult-yank-or-replace))
  (:map +insert-mode-map
        ("M-q" . #'+consult-kmacro))
  (:map project-prefix-map
        ("b" . #'consult-project-buffer))
  (:map consult-narrow-map
        ("C-h" . #'+consult-narrow-help))
  (:map vertico-map
        ("M-D" . #'+consult-grep-export-dired))
  (:map search-map
        ("t" . #'+consult-find-todos)
        ("M-t" . #'+consult-project-find-todos)
        ("b" . #'+consult-project-buffer)
        ("a" . #'+consult-org-agenda-todos)
        (".g" . #'+consult-ripgrep-here)
        (".f" . #'+consult-project-file-here)
        ("k" . #'+consult-find-key-binding)
        ("p" . #'+consult-find-package)
        ("K" . #'+consult-find-bound-function)))

(use-package consult-buffer-extras
  :ensure (:type file :main "~/.emacs.d/lisp/consult-buffer-extras.el" :files ("consult-buffer-extras.el"))
  :after project
  :bind
  (:map project-prefix-map
        ("e" . #'+consult-buffer-project-eshell))
  (:map +leader-map
        ("ba" . #'+consult-buffer-agent-shell)))

(use-package consult-eglot
  :bind
  (:map search-map
        ("s" . #'consult-eglot-symbols)))

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :bind
  (:map minibuffer-local-map
        ([remap kill-visual-line] . #'kill-line)
        ("C-k" . #'kill-line))
  (:map vertico-map
        ("C-;" . #'vertico-quick-exit)
        ("M-h" . #'vertico-directory-up)
        ("M-P" . #'+consult-toggle-preview)
        ("M-l" . #'vertico-directory-enter))
  :config
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (cl-loop for idx from 0 to 9
           do
           (define-key vertico-map (kbd (format "M-%d" idx))
                       `(lambda () (interactive)
                          (let ((vertico--index ,idx))
                            (call-interactively #'vertico-exit)))))

  (vertico-mode)
  (vertico-multiform-mode)
  (vertico-indexed-mode))

(use-package vertico-extras
  :ensure (:type file :main "~/.emacs.d/lisp/vertico-extras.el" :files ("vertico-extras.el"))
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . #'+vertico-exit)
        ("M-O" . #'+vertico-toggle-other-window-exit)
        ("M-N" . #'+vertico-toggle-new-window-exit)))

(use-package marginalia
  :defer 0.6
  :bind
  :config
  (marginalia-mode)
  (setf (alist-get 'function marginalia-annotators)
        '(marginalia-annotate-command marginalia-annotate-binding)))

(use-package embark-consult
  :after (consult embark))

(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-multiform-categories
   '((consult-location buffer (:not posframe))
     (consult-grep buffer (:not posframe))
     (imenu buffer (:not posframe))
     (t posframe)))
  (vertico-multiform-commands
   '((consult-line buffer (:not posframe))
     (consult-ripgrep buffer (:not posframe))
     (consult-git-grep buffer (:not posframe))
     (consult-grep buffer (:not posframe))))
  (vertico-posframe-fallback-mode #'vertico-buffer-mode)
  :init
  (vertico-posframe-mode))
