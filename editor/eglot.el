;;; -*- lexical-binding: t -*-
(use-package eglot
  :defer-incrementally
  (project eldoc flymake)
  :commands (eglot-shutdown)
  :hook
  ((go-mode
    java-mode java-ts-mode
    c++-mode c++-ts-mode
    elixir-mode
    typescript-mode typescript-ts-mode
    nix-mode
    tsx-ts-mode
    ;; js2-mode
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
  :config
  (setq eglot-events-buffer-size 1000000)  ; Log everything
  (setq eglot-sync-connect 3)              ; Wait longer for connection
  (advice-add 'eglot-rename :after
              (lambda (&rest _)
                (save-some-buffers t)))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(svelte-mode . ("svelteserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (add-to-list 'eglot-server-programs '(elixir-mode "~/.local/bin/elixir-ls/language_server.sh"))

  (custom-set-faces
   '(eglot-highlight-symbol-face ((t (:inherit nil)))))
  ;; https://github.com/joaotavora/eglot/discussions/1184
  (defun vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
      `(:typescript (:tsdk ,tsdk-path)
                    :vue (:hybridMode :json-false))))


  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))

(use-package eldoc
  :ensure nil
  :defer t
  :hook (eglot-mode . (lambda () (eldoc-mode -1)))
  :preface
  (when (and (version<= emacs-version "29.1") (featurep 'eldoc))
    (unload-feature 'eldoc t))
  (global-eldoc-mode -1)
  :config
  (setq eldoc-idle-delay 9999)
  (defun +eldoc-help ()
    "Show eldoc info for current symbol and restore cursor position."
    (interactive)
    (let ((win (selected-window)))
      (with-selected-window win
        (call-interactively #'eldoc-print-current-symbol-info)
        (run-with-timer 0.1 nil
                        (lambda ()
                          (select-window win)))))))

(unless (version<= emacs-version "29.1")
  (use-package dape
    :after hydra
    :defer t
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
  :general
  ('meow-normal-state-keymap xref--xref-buffer-mode-map
                             ";" #'xref-goto-xref))

(use-package jsonrpc
  :defer t
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package flymake
  :defer t
  :custom
  (flymake-no-changes-timeout 5)
  (flymake-show-diagnostics-at-end-of-line t))
