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
        ([remap insert-newline-indent] . #'exit-minibuffer)
        ([remap +keyboard-quit-normal] . #'abort-minibuffers)
        ("C-g" . #'abort-minibuffers)))

(use-package consult
  :defer 0.2
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (consult-narrow-key "C-SPC")
  :bind
  (:map +x-map
        ("b" . #'consult-buffer)
        ("'" . #'consult-recent-file))
  (:map +normal-mode-map
        ("M-g" . #'consult-goto-line))
  (:map minibuffer-mode-map
        ("M-r" . #'consult-history))
  (:map isearch-mode-map
        ("M-r" . #'consult-isearch-history))
  (:map +leader-map
        ("." . #'find-file)
        ("M-x" . #'consult-mode-command)
        ("fj" . #'consult-imenu)
        ("pm" . #'consult-global-mark)
        ("fb" . #'consult-bookmark)
        ("fm" . #'consult-mark)
        ("rc" . #'consult-complex-command)
        ;; ("fh" . #'consult-man)
        ("fe" . #'consult-flymake)
        ("fo" . #'consult-outline)
        ("f." . #'consult-fd)
        ("fl" . #'consult-goto-line)
        (";" . #'consult-ripgrep)
        ("C-;" . #'consult-ripgrep)
        ("pj" . #'consult-imenu-multi)))

(use-package consult-extensions
  :ensure (:type file :main "~/.emacs.d/packages/consult-extensions.el")
  :custom
  (consult-preview-excluded-buffers #'+consult-preview-tramp-excluded-p)
  :bind
  (:map +normal-mode-map
        ("M-q" . #'+consult-kmacro)
        ("M-y" . #'+consult-yank-or-replace))
  (:map +insert-mode-map
        ("M-q" . #'+consult-kmacro))
  (:map +leader-map
        ("," . #'+consult-project-buffer)
        ("ft" . #'+consult-find-todos)
        ("c;" . #'+consult-ripgrep-here)
        ("SPC bf" . #'+consult-unfocus-lines)
        ("SPC fm" . #'consult-minor-mode-menu)
        ("/" . #'+consult-line)
        ("bf" . #'+consult-focus-lines)
        ("fk" . #'+consult-find-key)
        ("cy" . #'+consult-yank-or-replace)
        ("SPC k" . #'+consult-keep-lines)
        ("fp" . #'+consult-find-package)
        ("SPC /" . #'+consult-line-multi)
        ("fa" . #'+consult-org-agenda-todos)
        ("pt" . #'+consult-project-find-todos)
        ("c'" . #'+consult-project-file-here)))

(use-package consult-eglot
  :hook (eglot-mode . consult-eglot-mode))

;; (use-package consult-dir
;;   :bind (("C-x C-d" . consult-dir)
;;          :map vertico-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))


(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :bind
  (:map minibuffer-local-map
        ([remap kill-visual-line] . #'kill-line)
        ("C-k" . #'kill-line))
  (:map vertico-map
        ("C-;" . #'vertico-quick-insert)
        ("M-h" . #'vertico-directory-up)
        ("M-P" . #'+consult-toggle-preview)
        ("M-l" . #'vertico-directory-enter))
  :config
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (add-to-list 'find-file-not-found-functions #'+auto-create-missing-dirs)

  (cl-loop for idx from 0 to 9
           do
           (define-key vertico-map (kbd (format "M-%d" idx))
                       `(lambda () (interactive)
                          (let ((vertico--index ,idx))
                            (call-interactively #'vertico-exit)))))

  (vertico-mode)
  (vertico-indexed-mode))

;;;###autoload
(defun +auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(use-package marginalia
  :defer 0.4
  :bind
  (:map minibuffer-local-map
        ("M-A" . #'marginalia-cycle))
  :config
  (marginalia-mode)
  (setf (alist-get 'function marginalia-annotators)
        '(marginalia-annotate-command marginalia-annotate-binding)))

(use-package embark-consult
  :after (consult embark))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode))
