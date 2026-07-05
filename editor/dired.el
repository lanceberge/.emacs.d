;;; -*- lexical-binding: t -*-
(use-package dired
  :ensure nil
  :after modal
  :defer 3.5
  :custom
  (dired-auto-revert-buffer)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-use-ls-dired nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-vc-rename-file t)
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map search-map
        (".d" . #'dired))
  (:map dired-mode-map
        ("i" . +dired-maybe-insert-subdir)
        ("g" . #'revert-buffer)
        ("e" . #'dired-toggle-read-only)
        ("!" . #'dired-do-eshell-command)
        ("-" . #'dired-up-directory))
  :config
  (+modal-bind '+motion-mode-map 'dired-mode-hook
               '(("x" . dired-do-flagged-delete)
                 ("g" . revert-buffer))))

;;;###autoload
(defun +dired-maybe-insert-subdir ()
  (interactive)
  (save-excursion
    (call-interactively #'dired-maybe-insert-subdir)))

(use-package wdired
  :ensure nil
  :bind
  (:map wdired-mode-map
        ([remap save-buffer] . +wdired-finish-edit)))

;;;###autoload
(defun +wdired-finish-edit ()
  (interactive)
  (wdired-finish-edit)
  (+normal-mode -1)
  (+insert-mode -1))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . #'dired-subtree-toggle)
        ("<backtab>" . #'dired-subtree-cycle)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . #'dired-rsync)))

(use-package dired-narrow
  :bind
  (:map dired-mode-map
        ("N" . #'dired-narrow)))

;; TODO maybe keep?
(use-package dired-filter
  :demand t
  :after dired
  :config
  (define-key dired-mode-map (kbd "f") dired-filter-map))

(use-package dired-collapse
  :after dired
  :demand t
  :config
  (global-dired-collapse-mode))

(use-package async
  :after dired
  :demand t
  :config
  (dired-async-mode))

(use-package diredfl
  :after dired
  :demand t
  :config
  (diredfl-global-mode))

(use-package dired-git-info
  :bind
  (:map dired-mode-map
        (")" . #'dired-git-info-mode)))

;;;###autoload
(defun dired-do-eshell-command (command)
  "Run an Eshell command on the marked files."
  (interactive "sEshell command: ")
  (let ((files (dired-get-marked-files t)))
    (eshell-command
     (format "%s %s" command (mapconcat #'identity files " ")))))
