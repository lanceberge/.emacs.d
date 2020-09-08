;;; -*- lexical-binding: t -*-
(defconst config-org (expand-file-name "README.org" user-emacs-directory))
(defconst config-el  (expand-file-name "config.el" user-emacs-directory))

(unless (file-exists-p config-el)
  (require 'org)
  (poly-org-mode -1)
  (org-babel-tangle-file config-org config-el))

(load-file config-el)
