;;; -*- lexical-binding: t -*-
(use-package eglot
  :defer-incrementally
  (project eldoc flymake)
  :hook
  ((go-mode
    java-mode java-ts-mode
    c++-mode c++-ts-mode
    typescript-mode typescript-ts-mode
    js2-mode
    python-base-mode
    sh-base-mode
    svelte-mode) . eglot-ensure)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-sync-connect nil)
  (eglot-events-buffer-size 0)
  :general
  ('normal
   "ga" #'eglot-code-actions
   "gh" #'eldoc-print-current-symbol-info)
  (my-localleader-def
    "gr" #'eglot-rename)
  :config
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))
  (setf (plist-get eglot-events-buffer-config :size) 0))

(use-package eldoc
  :ensure nil
  :preface
  (when (and (version<= emacs-version "29.1") (featurep 'eldoc))
    (unload-feature 'eldoc t))
  :defer t)

;; TODO
;; (when IS-MAC
;;   (use-package eglot-booster
;;     :ensure (:host github :repo "jdtsmith/eglot-booster")
;;     :after eglot
;;     :config
;;     (eglot-booster-mode -1)))

(unless (version<= emacs-version "29.1")
  (use-package dape
    :general
    (my-localleader-def
      "dd" (defun +dape ()
             (interactive)
             (call-interactively #'dape)
             (call-interactively #'dape-hydra/body))
      "db" #'dape-breakpoint-toggle
      "dh" #'dape-hydra/body)
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
  ('normal xref--xref-buffer-mode-map
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
