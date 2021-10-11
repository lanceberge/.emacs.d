;;; -*- lexical-binding: t -*-
(if (version< emacs-version "27.1")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(defconst config-org (expand-file-name "README.org" user-emacs-directory)
  "org-mode config to tangle into config.el")

(defconst config-el  (expand-file-name "config.el" user-emacs-directory)
  "emacs-lisp tangled config file")

(unless (file-exists-p config-el) ; tangle config-org to config-el
  (require 'org)
  (org-babel-tangle-file config-org config-el))

(load-file config-el) ; load tangled config file
