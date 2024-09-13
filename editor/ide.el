;;; -*- lexical-binding: t -*-
(use-package eglot
  ;; TODO eglot-booster
  :hook
  ((go-mode
    java-mode
    js2-mode
    python-mode
    svelte-mode
    typescript-ts-mode
    typescript-mode
    c++-mode
    c++-ts-mode) . eglot-ensure)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :general
  ('normal
   "ga" #'eglot-code-actions
   "gh" #'eldoc-print-current-symbol-info)
  (my-localleader-def
    "gr" #'eglot-rename))

(use-package dape
  :general
  (my-localleader-def
    "dd" #'dape
    "db" #'dape-breakpoint-toggle)
  :config
  ;; (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
  ;;     "
  ;; ^Stepping^          ^Breakpoints^               ^Info
  ;; ^^^^^^^^-----------------------------------------------------------
  ;; _d_: init           _bb_: Toggle (add/remove)   _si_: Info
  ;; _n_: Next           _bd_: Delete                _sm_: Memory
  ;; _i_: Step in        _bD_: Delete all            _ss_: Select Stack
  ;; _o_: Step out       _bl_: Set log message       _R_: Repl
  ;; _c_: Continue
  ;; _r_: Restart
  ;; _Q_: Disconnect
  ;; "
  ;;     ("d" dape)
  ;;     ("n" dape-next)
  ;;     ("i" dape-step-in)
  ;;     ("o" dape-step-out)
  ;;     ("c" dape-continue)
  ;;     ("r" dape-restart)
  ;;     ("ba" dape-breakpoint-toggle)
  ;;     ("bb" dape-breakpoint-toggle)
  ;;     ("be" dape-breakpoint-expression)
  ;;     ("bd" dape-breakpoint-remove-at-point)
  ;;     ("bD" dape-breakpoint-remove-all)
  ;;     ("bl" dape-breakpoint-log)
  ;;     ("si" dape-info)
  ;;     ("sm" dape-read-memory)
  ;;     ("ss" dape-select-stack)
  ;;     ("R"  dape-repl)
  ;;     ("q" nil "quit" :color blue)
  ;;     ("Q" dape-kill :color red))
  )

(use-package xref
  :commands (xref-find-references xref-auto-jump-first-definition)
  :custom
  (xref-prompt-for-identifier nil)
  :general
  ('normal xref--xref-buffer-mode-map
           ";" #'xref-goto-xref))

(use-package jsonrpc
  :defer t)

(use-package project
  :commands (project-switch-project)
  :general
  (my-leader-def
    "pp" #'(+project-switch-and-find-file :which-key "switch project")
    "pg" #'(+project-switch-and-magit-status :which-key "switch project")
    "pf" #'(project-find-file :which-key "find file")
    "ps" #'(consult-ripgrep :which-key "ripgrep")
    "p SPC p" #'(+project-switch-and-rg :which-key "switch project")))
