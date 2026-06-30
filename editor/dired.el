;;; -*- lexical-binding: t -*-
(use-package dired
  :defer t
  :ensure nil
  :after modal
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
        ("e" . #'dired-toggle-read-only)
        ("x" . #'dired-do-flagged-delete)
        ("-" . #'dired-up-directory))
  :config
  (+modal-bind +motion-mode-map dired-mode-hook
               ("x" . #'dired-do-flagged-delete)))

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
