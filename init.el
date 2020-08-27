;;; -*- lexical-binding: t -*-
(defconst config-org (expand-file-name "README.org" user-emacs-directory))
(defconst config-el  (expand-file-name "config.el" user-emacs-directory))

(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

(unless (file-exists-p config-el)
  (require 'org)
  (org-babel-tangle-file config-org config-el))

(load-file config-el)

(when (require 'time-date nil t)
  (message "Emacs startup time: %.2f seconds."
           (time-to-seconds (time-since emacs-load-start-time))))
