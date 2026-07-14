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
  (:map +normal-mode-map
        ("M-y" . #'consult-yank-pop))
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
        (":" . #'consult-complex-command))
  (:map minibuffer-mode-map
        ("M-r" . #'consult-history))
  (:map isearch-mode-map
        ("M-r" . #'consult-isearch-history))
  (:map project-prefix-map
        ("g" . #'consult-ripgrep)
        ("b" . #'consult-project-buffer)
        ("i" . #'consult-imenu-multi)
        ("l" . #'consult-line-multi))
  (:map help-map
        ("i" . #'consult-info)
        ("C-m" . #'consult-minor-mode-menu))
  (:map isearch-mode-map
        ("M-s l" . #'consult-line)
        ("M-s M-l" . #'consult-line-multi))
  (:map search-map
        ("g" . #'consult-ripgrep)
        ("r" . #'consult-recent-file)
        ("b" . #'consult-project-buffer)
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
        ("M-q" . #'+consult-kmacro))
  (:map +insert-mode-map
        ("M-q" . #'+consult-kmacro))
  (:map consult-narrow-map
        ("C-h C-h" . #'+consult-narrow-help))
  (:map vertico-map
        ("M-D" . #'+consult-grep-export-dired))
  (:map search-map
        ("t" . #'+consult-find-todos)
        ("M-t" . #'+consult-project-find-todos)
        ("a" . #'+consult-org-agenda-todos)
        (".g" . #'+consult-ripgrep-here)
        (".f" . #'+consult-project-file-here)
        ("k" . #'+consult-find-key-binding)
        ("p" . #'+consult-find-package)
        ("K" . #'+consult-find-bound-function)))

(use-package consult-eglot
  :bind
  (:map search-map
        ("s" . #'consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after consult-eglot
  :demand t
  :config
  (consult-eglot-embark-mode))

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
        ("C-<return>" . #'vertico-quick-embark)
        ("S-<return>" . #'vertico-quick-exit)
        ("M-h" . #'vertico-directory-up)
        ("M-P" . #'+consult-toggle-preview)
        ("M-l" . #'vertico-directory-enter))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  (cl-loop for idx from 0 to 9
           do
           (define-key vertico-map (kbd (format "M-%d" idx))
                       `(lambda () (interactive)
                          (let ((vertico--index ,idx)
                                (this-command #'vertico-exit))
                            (call-interactively #'vertico-exit)))))

  (vertico-mode)
  (vertico-multiform-mode)
  (vertico-indexed-mode))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure nil
  :after (vertico prescient)
  :custom
  (vertico-prescient-enable-filtering nil)
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  :config
  (vertico-prescient-mode))

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

(use-package consult-dir
  :custom
  (consult-dir-sort-candidates t)
  (consult-dir-default-command #'project-find-file)
  :bind
  (:map goto-map
        ("d" . #'consult-dir)))

(use-package ctrlf)

;; TODO integrate with consult-vc
(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-omni
  :unless IS-WORK2
  :ensure (:host github :repo "armindarvish/consult-omni"
                 :files (:defaults "sources/*.el"))
  :after (consult)
  :config
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)

  (setq consult-omni-sources-modules-to-load
        '(consult-omni-duckduckgo
          consult-omni-wikipedia
          consult-omni-dict
          consult-omni-elfeed
          consult-omni-gptel))

  (consult-omni-sources-load-modules)

  (setq consult-omni-multi-sources
        '("DuckDuckGo API"
          "Wikipedia"
          "Dictionary"
          "elfeed"))

  (setq consult-omni-default-interactive-command #'consult-omni-multi)

  (setq consult-omni-show-preview t)
  (setq consult-omni-preview-key "C-o"))
