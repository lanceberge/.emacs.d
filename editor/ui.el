;;; -*- lexical-binding: t -*-
(use-package frameset
  :ensure nil
  :defer 0.8
  :config
  (setq frameset-filter-alist
        (append '((background-color . :never)
                  (foreground-color . :never)
                  (background-mode . :never)
                  (cursor-color . :never)
                  (mouse-color . :never)
                  (border-color . :never)
                  (scroll-bar-foreground . :never)
                  (scroll-bar-background . :never))
                frameset-filter-alist)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package transient
  :bind
  (:map transient-base-map
        ("M-p" . #'transient-reset))
  :config
  (transient-bind-q-to-quit))

(use-package paren ; show matching parentheses
  :ensure nil
  :hook ((prog-mode text-mode) . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-mode ; higlight hex codes, colors with the color
  :custom (rainbow-x-colors nil) ; don't highlight white, blue, etc.
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package fringe
  :ensure nil)

;;;###autoload
(defun +indent-bars--inside-heex-sigil-p (pos)
  "Return non-nil when POS is inside an Elixir ~H HEEx sigil."
  (and (fboundp 'treesit-language-at)
       (eq (treesit-language-at pos) 'heex)
       (let ((node (treesit-node-at pos 'elixir)))
         (while (and node
                     (not (string= (treesit-node-type node) "quoted_content")))
           (setq node (treesit-node-parent node)))
         (when-let* ((name (and node (treesit-node-prev-sibling node t))))
           (and (string= (treesit-node-type name) "sigil_name")
                (string= (treesit-node-text name t) "H"))))))

;;;###autoload
(defun +indent-bars--display-heex-only (beg end &rest args)
  "Draw indent bars only inside Elixir ~H sigils."
  (when (+indent-bars--inside-heex-sigil-p beg)
    (apply #'indent-bars--display beg end args)))

;;;###autoload
(defun +indent-bars--display-heex-blank-lines-only (beg end &rest args)
  "Draw blank-line indent bars only inside Elixir ~H sigils."
  (when (+indent-bars--inside-heex-sigil-p beg)
    (apply #'indent-bars--display-blank-lines beg end args)))

;;;###autoload
(defun +indent-bars-elixir-heex-mode ()
  "Enable `indent-bars-mode' only for HEEx content inside Elixir ~H sigils."
  (setq-local indent-bars-no-descend-string nil
              indent-bars-no-descend-lists nil
              indent-bars-spacing-override 2
              indent-bars--display-blank-lines-function #'+indent-bars--display-heex-blank-lines-only)
  indent-bars--display-function #'+indent-bars--display-heex-only
  (indent-bars-mode 1))

(use-package indent-bars
  :hook
  (elixir-web-mode . +indent-bars-elixir-heex-mode)
  ((nxml-mode web-mode) . indent-bars-mode))

(use-package kanagawa-themes
  :ensure (:host github :repo "lanceberge/kanagawa-emacs"))

(use-package doom-themes)

(use-package theme-rotate
  :ensure (:type file :main "~/.emacs.d/lisp/theme-rotate.el")
  :hook (emacs-startup . +theme-rotate-load-current-theme)
  :custom
  (+theme-rotate-dark-themes
   '(gruvbox-dark-hard kanagawa-wave doom-badger doom-oceanic-next
                       doom-tomorrow-night doom-spacegrey doom-tokyo-night
                       doom-palenight doom-Iosvkem doom-one doom-dark+
                       doom-monokai-octagon))
  (+theme-rotate-light-themes
   '(tango kanagawa-paper doom-nord-light doom-oksolar-light
           doom-solarized-light tsdh-light doom-opera-light))
  (+theme-rotate-current-style 'dark)
  :bind
  (:repeat-map +theme-rotate-repeat-map
               ("]" . #'+theme-rotate-rotate)
               ("t" . #'+theme-rotate-toggle-dark-light)
               ("[" . #'+theme-rotate-prev))
  (:map +normal-mode-map
        ("]T" . #'+theme-rotate-rotate)
        ("[T" . #'+theme-rotate-prev))
  (:map +leader-map
        ("tt" . #'+theme-rotate-toggle-dark-light)))

(use-package consult-theme-rotate
  :ensure (:type file :main "~/.emacs.d/lisp/consult-theme-rotate.el")
  :after (consult theme-rotate)
  :bind
  (:map +leader2-map
        ("tf" . #'+consult-theme-rotate))
  (:map +consult-theme-rotate-minibuffer-mode-map
        ("M-P" . #'+consult-theme-rotate-toggle-style)))

(use-package theme-rotate-omarchy
  :defer 1.5
  :ensure (:type file :main "~/.emacs.d/lisp/theme-rotate-omarchy.el"))
