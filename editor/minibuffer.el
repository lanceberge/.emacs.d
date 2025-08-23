;;; -*- lexical-binding: t -*-
(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . meow-insert-mode)
  (minibuffer-setup . undo-tree-mode)
  :bind
  (:map minibuffer-mode-map
        ([remap newline] . #'exit-minibuffer)
        ("C-g" . #'abort-minibuffers)))

(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :bind
  (:map meow-normal-state-keymap
        ("M-q" . #'consult-kmacro))
  (:map meow-insert-state-keymap
        ("M-q" . #'consult-kmacro))
  (:map +leader-map
        ("," . #'+project-buffer)
        ("." . #'find-file)
        ("/" . #'consult-line)
        ("pl" . #'consult-line-multi)
        ("pg" . #'consult-git-grep)
        ("fr" . #'consult-recent-file)
        ("bj" . #'consult-imenu)
        ("bb" . #'consult-buffer)
        ("fm" . #'consult-global-mark)
        ("fb" . #'consult-bookmark)
        ("bm" . #'consult-mark)
        ("y" . #'consult-yank-from-kill-ring)
        ("f SPC y" . #'consult-yank-replace)
        ("fh" . #'consult-man)
        ("ft" . #'find-todos)
        ("fe" . #'consult-flymake)
        ("fo" . #'consult-outline)
        ("f." . #'consult-find)
        ("fl" . #'consult-goto-line)
        ("fa" . #'consult-org-agenda)
        ("fs" . #'consult-ripgrep)
        ("fj" . #'consult-imenu-multi)))

;;;###autoload
(defun +project-buffer ()
  (interactive)
  (if (project-root (project-current t))
      (consult-project-buffer)
    (consult-buffer)))
;;;###autoload
(defun +find-todos ()
  "Search all todos."
  (interactive)
  (consult-line "TODO"))

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :bind
  (:map vertico-map
        ("C-j" . #'vertico-next)
        ("C-k" . #'vertico-previous)
        ("C-u" . #'vertico-scroll-down)
        ("C-d" . #'vertico-scroll-up)
        ("M-j" . #'vertico-next)
        ("M-k" . #'vertico-previous)
        ("M-u" . #'vertico-scroll-down)
        ("M-d" . #'vertico-scroll-up)
        ("M-h" . #'vertico-directory-up)
        ("M-l" . #'vertico-directory-enter))
  :config
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
;;;###autoload
  (defun +auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions #'+auto-create-missing-dirs)

  (cl-loop for idx from 0 to 9
           do
           (define-key vertico-map (kbd (format "M-%d" idx))
                       `(lambda () (interactive)
                          (let ((vertico--index ,idx))
                            (call-interactively #'vertico-exit)))))

  (vertico-mode)
  (vertico-indexed-mode))

(use-package marginalia
  :defer 0.4
  :config
  (marginalia-mode)
  (setq marginalia-annotators
        (seq-remove (lambda (x) (memq (car x) '(tab file project-file)))
                    marginalia-annotators))
  (setf (alist-get 'function marginalia-annotators)
        '(marginalia-annotate-command . marginalia-annotate-binding)))

(use-package embark-consult
  :after (consult embark))
