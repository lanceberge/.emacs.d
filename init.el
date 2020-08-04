(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-enable-at-startup nil
      load-prefer-newer t)
(package-initialize)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(garbage-collect)
