;;; -*- lexical-binding: t -*-
(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . meow-insert-mode)
  :custom
  (enable-recursive-minibuffers t)
  :bind
  (:map minibuffer-inactive-mode-map
        ([remap save-buffer] . #'kill-current-buffer))
  (:map minibuffer-mode-map
        ([remap newline] . #'exit-minibuffer)
        ([remap insert-newline-indent] . #'exit-minibuffer)
        ("C-g" . #'abort-minibuffers)))

(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (consult-narrow-key "C-SPC")
  :bind (
         :map meow-normal-state-keymap
         ("M-q" . #'consult-kmacro)
         ("M-y" . #'+consult-yank-or-replace)
         :map meow-insert-state-keymap
         ("M-q" . #'consult-kmacro)
         :map minibuffer-mode-map
         ("M-r" . #'consult-history)
         :map isearch-mode-map
         ("M-r" . #'consult-isearch-history)
         :map +leader-map
         ("," . #'+project-buffer)
         ("." . #'find-file)
         ("SPC bf" . #'+consult-unfocus-lines)
         ("SPC fm" . #'consult-minor-mode-menu)
         ("/" . #'+consult-line)
         ("SPC /" . #'+consult-line-multi)
         ("fr" . #'consult-recent-file)
         ("fj" . #'consult-imenu)
         ("bb" . #'consult-buffer)
         ("bf" . #'+consult-focus-lines)
         ("pm" . #'consult-global-mark)
         ("fb" . #'consult-bookmark)
         ("fm" . #'consult-mark)
         ("SPC fy" . #'consult-yank-replace)
         ("SPC k" . #'+consult-keep-lines)
         ("rf" . #'consult-recent-file)
         ("rc" . #'consult-complex-command)
         ("fk" . #'+find-key)
         ("fh" . #'consult-man)
         ("ft" . #'+find-todos)
         ("fe" . #'consult-flymake)
         ("fo" . #'consult-outline)
         ("f." . #'consult-find)
         ("fl" . #'consult-goto-line)
         ("fa" . #'consult-org-agenda)
         ("ps" . #'consult-ripgrep)
         (";" . #'consult-ripgrep)
         ("fs" . #'+consult-ripgrep-current)
         ("SPC ;" . #'+consult-ripgrep-current)
         ("pj" . #'consult-imenu-multi)
         :map org-mode-map
         ([remap consult-imenu] . #'consult-org-heading)))

;;;###autoload
(defun +consult-focus-lines ()
  (interactive)
  (forward-char 1)
  (call-interactively #'consult-focus-lines))

;;;###autoload
(defun +consult-keep-lines ()
  (interactive)
  (forward-char 1)
  (call-interactively #'consult-keep-lines))

;;;###autoload
(defun +consult-yank-or-replace ()
  "If region is active, replace it with selected text from kill ring using consult-yank-pop.
Otherwise, just call consult-yank-pop."
  (interactive)
  (if (region-active-p)
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (defun +consult-yank-replace-region (&rest _)
          (when (region-active-p)
            (delete-region region-start region-end)))
        (unwind-protect
            (progn
              (add-function :after (symbol-function 'consult-yank-pop) #'+consult-yank-replace-region)
              (call-interactively #'consult-yank-pop))
          (remove-function (symbol-function 'consult-yank-pop) #'+consult-yank-replace-region)))
    (call-interactively #'consult-yank-pop)))

;;;###autoload
(defun +consult-unfocus-lines ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively #'consult-focus-lines)))

;;;###autoload
(defun +consult-line ()
  (interactive)
  (call-interactively #'consult-line)
  (let ((consult-search (car consult--line-history)))
    (unless (string-match-p " " consult-search)
      (+isearch-update-last-search consult-search))))

;;;###autoload
(defun +consult-line-multi ()
  (interactive)
  (call-interactively #'consult-line-multi)
  (let ((consult-search (car consult--line-multi-history)))
    (unless (string-match-p " " consult-search)
      (+isearch-update-last-search consult-search))))

;;;###autoload
(defun +consult-ripgrep-current ()
  (interactive)
  (consult-ripgrep default-directory nil))

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
        ("C-f" . #'vertico-quick-insert)
        ("C-k" . #'vertico-previous)
        ("C-u" . #'vertico-scroll-down)
        ("C-d" . #'vertico-scroll-up)
        ([remap drag-stuff-down] . #'vertico-next)
        ([remap drag-stuff-up] . #'vertico-previous)
        ("M-u" . #'vertico-scroll-down)
        ("M-d" . #'vertico-scroll-up)
        ("M-h" . #'vertico-directory-up)
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

(defun +find-key (key-sequence)
  "Goto the definition of the command bound to `KEY-SEQUENCE'"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  (key-description sym)))
     ((commandp sym t)
      (xref-find-definitions (symbol-name sym)))
     (t
      (user-error "%s is bound to %s which is not a command"
                  (key-description key-sequence)
                  sym)))))
