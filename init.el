;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

(defconst config-org (expand-file-name "README.org" user-emacs-directory))
(defconst config-el  (expand-file-name "config.el" user-emacs-directory))

(unless (file-exists-p config-el)
  (require 'org)
  (org-babel-tangle-file config-org config-el))

(load-file config-el)
