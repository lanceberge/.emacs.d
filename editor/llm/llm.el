;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-sonnet-4-6)
  (gptel-default-mode 'org-mode)
  (gptel-cache '(system message tool))
  :config
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(use-package gptel-extras
  :ensure (:type file :main "~/.emacs.d/lisp/gptel-extras.el" :files ("gptel-extras.el"))
  :after gptel
  :demand t
  :bind
  (:map gptel-mode-map
        ("C-c C-l". #'+gptel-project-clear-buffer)
        ([remap org-return] . #'+gptel-send)
        (["C-j"] . #'newline)
        ([remap gptel-send] . #'+gptel-send)
        ("S-<return>" . #'newline))
  (:map +llm-map
        ("g" . #'+gptel-project))
  :config
  (add-hook 'gptel-rewrite-directives-hook #'+gptel-rewrite-directive-from-agents-md))

(use-package agent-lisp
  :ensure (:type file :main "~/.emacs.d/lisp/agent-lisp.el" :files ("agent-lisp.el"))
  :commands
  (+agent-lisp-trace +agent-lisp-apropos +agent-lisp-eval-buffer +agent-lisp-function-source))

(use-package eca
  :custom
  (eca-chat-use-side-window nil)
  (eca-custom-command (list eca-server-install-path "server"))
  :bind
  (:map +llm-map
        ("e" . #'eca)))
