;;; -*- lexical-binding: t -*-
(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :general
  (my-leader-def
    "," #'(consult-buffer :which-key "find buffer")
    "." #'(find-file :which-key "find file")
    "/" #'(consult-line :which-key "line")
    "pl" #'(consult-line-multi :which-key "project line")
    "fr" #'(consult-recent-file :which-key "find recent file")
    "fj" #'(consult-imenu :which-key "imenu")
    "pb" #'(consult-project-buffer :which-key "project buffer")
    "fm" #'(consult-global-mark :which-key "mark")
    "bm" #'(consult-mark :which-key "buffer mark")
    "fb" #'(consult-bookmark :which-key "find bookmark")
    "fy" #'(consult-yank-from-kill-ring :which-key "yank")
    "f SPC y" #'(consult-yank-replace :which-key "replace yank")
    "fh" #'(consult-man :which-key "help")
    "ft" (defun +find-todos ()
           "Search all todos."
           (interactive)
           (consult-line "TODO"))
    "fe" #'consult-flymake
    "fo" #'(consult-outline :which-key "outline")
    "f." #'(consult-find :which-key "file")
    "fl" #'(consult-goto-line :which-key "outline")
    "fa" #'(consult-org-agenda :which-key "agenda")
    "fs" #'(consult-ripgrep :which-key "ripgrep")
    "f SPC j" #'(consult-imenu-multi :which-key "imenu"))
  ([remap projectile-find-file] #'(consult-project-buffer :which-key "project buffer"))

  ('isearch-mode-map
   "/" #'consult-line))

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :general
  ('(vertico-map minibuffer-inactive-mode-map)
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-u" #'vertico-scroll-down
   "C-d" #'vertico-scroll-up
   "M-j" #'vertico-next
   "M-k" #'vertico-previous
   "M-u" #'vertico-scroll-down
   "M-d" #'vertico-scroll-up
   "M-h" #'vertico-directory-up
   "M-l" #'vertico-directory-enter)
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

(use-package isearch
  :ensure nil
  :config
  (setq search-nonincremental-instead nil))
