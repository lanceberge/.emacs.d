;;; -*- lexical-binding: t -*-
(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer)
  (dired-recursive-copies 'always)
  :bind
  (:map +leader-map
        ("-" . #'dired-jump))
  (:map dired-mode-map
        ("i" . dired-toggle-read-only)
        ([remap meow-line] . dired-do-flagged-delete)
        ([remap negative-argument] . #'+dired/up-dir))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;;;###autoload
(defun +dired/up-dir ()
  "navigate up a directory in dired in the same buffer"
  (interactive)
  (find-alternate-file ".."))

(use-package wdired
  :ensure nil
  :bind
  (:map wdired-mode-map
        ([remap save-buffer] . wdired-finish-edit)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "^\\..$\\|^.$")) ; hide .. and ../ in dired
