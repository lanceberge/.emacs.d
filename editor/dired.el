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
  (dired-vc-rename-file t)
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
        ("i" . +dired-maybe-insert-subdir)
        ("e" . #'dired-toggle-read-only)
        ("x" . #'dired-do-flagged-delete)
        ([remap negative-argument] . #'+dired/up-dir))
  :config
  (+modal-bind +motion-mode dired-mode-hook
               "x" #'dired-do-flagged-delete)
  (put 'dired-find-alternate-file 'disabled nil))

;;;###autoload
(defun +dired/up-dir ()
  "navigate up a directory in dired in the same buffer"
  (interactive)
  (find-alternate-file ".."))

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
  (+normal-mode -1))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired
