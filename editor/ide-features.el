;;; -*- lexical-binding: t -*-
(use-package eglot
  :defer 1.4
  :commands (eglot-shutdown)
  :hook
  ((go-mode
    java-mode java-ts-mode
    c++-mode c++-ts-mode
    elixir-mode elixir-ts-mode
    typescript-mode typescript-ts-mode
    nix-mode
    tsx-ts-mode
    js2-mode
    python-base-mode
    php-mode
    rust-mode rust-ts-mode
    sh-base-mode
    scala-mode
    svelte-mode
    vue-mode) . eglot-ensure)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  :bind
  (:map +leader2-map
        ("er" . #'eglot-reconnect)
        ("r" . #'eglot-rename)
        ("a" . #'eglot-code-actions))
  :config
  (setq eglot-sync-connect 3) ; Wait longer for connection

  ;; save buffers after eglot-renaming
  (advice-add 'eglot-rename :after
              (lambda (&rest _)
                (save-some-buffers t)))

  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (let ((elixir-lsp-path (if (eq system-type 'darwin)
                             "~/.local/bin/expert_darwin_arm64"
                           "~/.local/bin/expert_linux_amd64")))
    (add-to-list 'eglot-server-programs
                 (list '(elixir-mode elixir-ts-mode heex-ts-mode)
                       elixir-lsp-path "--stdio")))

  (custom-set-faces
   '(eglot-highlight-symbol-face ((t (:inherit nil)))))

  (add-to-list 'eglot-server-programs '(python-ts-mode . ("/opt/homebrew/bin/pyright-langserver" "--stdio")))

  ;; https://github.com/joaotavora/eglot/discussions/1184
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))

;;;###autoload
(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
                    "lib"
                    (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
    `(:typescript (:tsdk ,tsdk-path)
                  :vue (:hybridMode :json-false))))

(unload-feature 'eldoc t)
(defvar global-eldoc-mode nil)

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 1)
  ;; https://www.reddit.com/r/emacs/comments/1lbo5jy/comment/myig4p7/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (eglot-code-action-indications '(eldoc-hint)))

;;;###autoload
(defun +eldoc-elp ()
  "Show eldoc info for current symbol and restore cursor position."
  (interactive)
  (let ((win (selected-window)))
    (with-selected-window win
      (call-interactively #'eldoc-print-current-symbol-info)
      (run-with-timer 0.1 nil
                      (lambda ()
                        (select-window win))))))

(unless (version<= emacs-version "29.1")
  (use-package dape
    :after hydra
    :config
    (remove-hook 'dape-start-hook 'dape-info)
    (remove-hook 'dape-start-hook 'dape-repl)
    (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
              "
  ^Stepping^          ^Breakpoints^               ^Info
  ^^^^^^^^-----------------------------------------------------------
  _d_: init           _bb_: Toggle (add/remove)   _si_: Info
  _n_: Next           _bd_: Delete                _sm_: Memory
  _i_: Step in        _bD_: Delete all            _ss_: Select Stack
  _o_: Step out       _bl_: Set log message       _R_: Repl
  _c_: Continue
  _r_: Restart
  _Q_: Disconnect
  "
              ("d" dape)
              ("n" dape-next)
              ("i" dape-step-in)
              ("o" dape-step-out)
              ("c" dape-continue)
              ("r" dape-restart)
              ("ba" dape-breakpoint-toggle)
              ("bb" dape-breakpoint-toggle)
              ("be" dape-breakpoint-expression)
              ("bd" dape-breakpoint-remove-at-point)
              ("bD" dape-breakpoint-remove-all)
              ("bl" dape-breakpoint-log)
              ("si" dape-info)
              ("sm" dape-read-memory)
              ("ss" dape-select-stack)
              ("R"  dape-repl)
              ("q" nil "quit" :color blue)
              ("Q" dape-kill :color red))))

(use-package xref
  :commands (xref-find-references xref-auto-jump-first-definition)
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package jsonrpc
  :config
  (defvar +eglot--jsonrpc-log-event-default nil
    "Default function used for `jsonrpc--log-event'.")
  (when (or (null +eglot--jsonrpc-log-event-default)
            (symbolp +eglot--jsonrpc-log-event-default))
    (setq +eglot--jsonrpc-log-event-default
          (symbol-function 'jsonrpc--log-event)))


  (fset 'jsonrpc--log-event #'ignore))

;;;###autoload
(defun +eglot-toggle-debug ()
  "Toggle Eglot JSONRPC event logging."
  (interactive)
  (if (eq (symbol-function 'jsonrpc--log-event) #'ignore)
      (progn
        (fset 'jsonrpc--log-event +eglot--jsonrpc-log-event-default)
        (message "Eglot JSONRPC debug logging enabled"))
    (fset 'jsonrpc--log-event #'ignore)
    (message "Eglot JSONRPC debug logging disabled")))

(use-package flymake
  :custom
  (flymake-no-changes-timeout 5)
  (flymake-show-diagnostics-at-end-of-line nil)
  :bind
  (:repeat-map flymake-repeat-map
               ("n" . #'flymake-goto-next-error)
               ("p" . #'flymake-goto-prev-error))
  (:map +leader2-map
        ("en" . #'flymake-goto-next-error)
        ("ep" . #'flymake-goto-prev-error)))

(use-package flyover
  :ensure t
  :hook ((flycheck-mode . flyover-mode)
         (flymake-mode . flyover-mode))
  :custom
  ;; Checker settings
  (flyover-checkers '(flymake))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)

  ;; Text tinting
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icon tinting (foreground and background)
  (flyover-icon-tint 'lighter)
  (flyover-icon-tint-percent 50)
  (flyover-icon-background-tint 'darker)
  (flyover-icon-background-tint-percent 50)

  ;; Icons
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon " ")

  ;; Border styles: none, pill, arrow, slant, slant-inv, flames, pixels
  (flyover-border-style 'pill)
  (flyover-border-match-icon t)

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2)
  (flyover-cursor-debounce-interval 0.3)

  ;; Display mode (controls cursor-based visibility)
  (flyover-display-mode 'always)

  ;; Completion integration
  (flyover-hide-during-completion t))

(use-package breadcrumb)

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :hook (prog-mode . +dumb-jump-maybe-enable)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

;;;###autoload
(defun +dumb-jump-xref-activate-unless-eglot ()
  "Return the dumb-jump xref backend unless eglot is managing the buffer."
  (unless (bound-and-true-p eglot--managed-mode)
    (dumb-jump-xref-activate)))

;;;###autoload
(defun +dumb-jump-maybe-enable ()
  "Enable dumb-jump as an xref backend in non-elisp `prog-mode' buffers."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (add-hook 'xref-backend-functions
              #'+dumb-jump-xref-activate-unless-eglot nil t)))

;;;###autoload
(defun +xref-rename-symbol-at-point ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (identifier (or symbol (user-error "No symbol at point")))
         (to (read-string (format "Replace '%s' with: " symbol))))
    (when (string-equal symbol to)
      (message "No change needed")
      (cl-return-from my/elixir-rename-symbol-at-point))
    (let ((xref-show-xrefs-function
           (lambda (fetcher _display-action)
             (funcall fetcher)))
          xrefs)
      (setq xrefs (xref-find-references identifier))
      (if (not xrefs)
          (message "No references found for '%s' — nothing to rename" identifier)
        (let ((count 0))
          (dolist (xref xrefs)
            (let* ((loc (xref-item-location xref))
                   (marker (xref-location-marker loc)))
              (when (and marker (buffer-live-p (marker-buffer marker)))
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (let ((bounds (bounds-of-thing-at-point 'symbol)))
                      (when bounds
                        (delete-region (car bounds) (cdr bounds))
                        (insert to)
                        (setq count (1+ count)))))))))
          (message "Replaced '%s' → '%s' (%d occurrences)" symbol to count))))))
