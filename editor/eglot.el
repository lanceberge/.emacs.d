;;; -*- lexical-binding: t -*-
(use-package eglot
  :defer-incrementally
  (project eldoc flymake)
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
  ;; https://github.com/joaotavora/eglot/discussions/1184
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("/opt/homebrew/bin/pyright-langserver" "--stdio")))

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
  :bind
  (:map meow-normal-state-keymap
        ("C-t" . #'pop-tag-mark)))

(use-package jsonrpc
  :config
  (fset #'jsonrpc--log-event #'ignore))

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
