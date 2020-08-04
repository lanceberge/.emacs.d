;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold most-positive-fixnum)
      ;; gc-cons-percentage 0.6)

(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216
		  gc-cons-percentage 0.1
		  file-name-handler-alist default-file-name-handler-alist)))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
