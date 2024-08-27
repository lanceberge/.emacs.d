;;; -*- lexical-binding: t -*-
(use-package consult
  :defer 0.2
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :general
  (
   [remap projectile-find-file] #'(consult-project-buffer :which-key "project buffer"))

  (my-leader-def
    "SPC"     #'(consult-buffer         :which-key "find buffer")
    "."       #'(find-file              :which-key "find file")
    "/"       #'(consult-line           :which-key "line")
    "fr"      #'(consult-recent-file    :which-key "find recent file")
    "fj"      #'(consult-imenu          :which-key "imenu")
    "fp"      #'(consult-project-buffer :which-key "project buffer")
    "f SPC j" #'(consult-imenu-multi    :which-key "imenu")
    "fm"      #'(consult-global-mark    :which-key "mark")
    "fp"      #'(consult-project-buffer :which-key "project buffer")
    "fo"      #'(consult-outline        :which-key "outline")
    "f."      #'(consult-find           :which-key "file")
    "fl"      #'(consult-goto-line      :which-key "outline")
    "fa"      #'(consult-org-agenda     :which-key "agenda")
    "fs"      #'(consult-ripgrep        :which-key "ripgrep")
    )

  ('org-agenda-mode-map
   [remap evil-search-forward] #'(consult-line :which-key "line"))

  ('isearch-mode-map
   "/" #'consult-line)
  )

(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :general
  ('(normal insert) '(vertico-map minibuffer-inactive-mode-map)
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-u" #'vertico-scroll-down
   "C-d" #'vertico-scroll-up
   "M-j" #'vertico-next
   "M-k" #'vertico-previous
   "M-u" #'vertico-scroll-down
   "M-d" #'vertico-scroll-up
   ";"   #'vertico-exit
   "M-h" #'vertico-directory-up
   "M-l" #'vertico-directory-enter
   "M-l" #'vertico-directory-enter
   "C-w" #'evil-delete-backward-word
   )
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

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark-consult
  :after (consult embark))

(use-package yasnippet ; snippets
  ;; https://joaotavora.github.io/yasnippet/snippet-development.html
  :defer 0.7
  :defer-incrementally (easymenu help-mode yasnippet-snippets)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :general
  ('yas-keymap
   "<tab>" #'yas-next-field)
  ('visual 'org-mode-map
           "ss" (defun +src-snippet () (interactive) (+expand-snippet "highlighted src"))
           )
  ('visual 'prog-mode-map
           "st" (defun +try-catch-snippet () (interactive) (+expand-snippet "try-catch"))
           )

  ('snippet-mode-map
   "C-c C-c" #'+yas-load-snippet-noconfirm)
  (my-leader-def
    "si" #'(yas-insert-snippet     :which-key "insert")
    "sn" #'(yas-new-snippet        :which-key "new")
    "sf" #'(yas-visit-snippet-file :which-key "find snippet")
    "sl" #'(yas-describe-tables    :which-key "list")
    "sr" #'(yas-reload-all         :which-key "reload"))
  :config
  ;; https://github.com/emacs-evil/evil/issues/254
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m)))))

;;;###autoload
  (defun +yas-load-snippet-noconfirm()
    (interactive)
    (unless yas--guessed-modes
      (setq-local yas--guessed-modes (yas--compute-major-mode-and-parents buffer-file-name)))
    (let ((template (yas-load-snippet-buffer (cl-first yas--guessed-modes) t)))
      (when (buffer-modified-p)
        (let ((default-directory (car (cdr (car (yas--guess-snippet-directories
                                                 (yas--template-table template))))))
              (default-file-name (yas--template-name template)))
          (setq buffer-file-name (concat default-directory default-file-name))
          (rename-buffer default-file-name t)
          (save-buffer)))
      (quit-window t)))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer t
  :config
  (yas--remove-template-by-uuid (yas--table-get-create 'emacs-lisp-mode) "kill-buffer"))

(use-package corfu
  :defer 1.4
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect 'first)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  :general
  ('corfu-map
   "RET" #'newline
   "C-k" #'corfu-previous
   "M-j" #'corfu-next
   "M-k" #'corfu-previous
   "C-j" #'corfu-next
   ";"   #'corfu-complete
   "<tab>" #'yas-expand
   )
  ('insert corfu-map
           "C-k" #'corfu-previous)
  :config
  (global-corfu-mode)
  (advice-add 'evil-escape-func :after #'corfu-quit))

(use-package cape
  :after corfu
  :hook
  (text-mode        . +cape-text-mode)
  (minibuffer-setup . +cape-minibuffer-mode)
  :custom
  (cape-file-directory-must-exist nil)
  :init
  (defun +cape-text-mode ()
    (setq-local corfu-auto-prefix 4))

  (defun +cape-minibuffer-mode ()
    (dolist (backend '(cape-history))
      (add-to-list 'completion-at-point-functions backend))
    (setq-local corfu-auto-prefix 3))

  (dolist (backend '(cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))
