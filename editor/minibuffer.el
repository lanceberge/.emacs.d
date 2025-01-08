;;; -*- lexical-binding: t -*-
(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :general
  ([remap projectile-find-file] #'(consult-project-buffer :which-key "project buffer"))

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
    "fp" (defun +find-package ()
           "Search all use-packages in .emacs.d."
           (interactive)
           (consult-ripgrep "~/.emacs.d/" "use-package "))
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

  ('org-agenda-mode-map
   [remap evil-search-forward] #'(consult-line :which-key "line"))

  ('isearch-mode-map
   "/" #'consult-line))

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :general
  ('(normal insert) '(vertico-map minibuffer-inactive-mode-map)
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-h" #'vertico-directory-up
   "C-l" #'vertico-directory-enter
   "C-u" #'vertico-scroll-down
   "C-d" #'vertico-scroll-up
   "M-j" #'vertico-next
   "M-k" #'vertico-previous
   "M-u" #'vertico-scroll-down
   "M-d" #'vertico-scroll-up
   ";" #'vertico-exit
   "M-h" #'vertico-directory-up
   "M-l" #'vertico-directory-enter
   "C-w" #'evil-delete-backward-word)
  :config
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
;;;###autoload
  (defun +auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions #'+auto-create-missing-dirs)

  (evil-collection-init 'minibuffer)
  (vertico-mode))

(use-package marginalia
  :defer 0.4
  :config
  (marginalia-mode))

(use-package embark-consult
  :after (consult embark))
