;;; -*- lexical-binding: t -*-
(use-package agent-shell
  :custom
  (agent-shell-show-config-icons nil)
  (agent-shell-preferred-agent-config 'codex)
  (agent-shell-header-style nil)
  :commands (agent-shell-openai-start-codex)
  :bind
  (:map +llm-map
        ("a" . #'agent-shell-new-shell))
  (:map agent-shell-mode-map
        ("C-c C-l" . #'agent-shell-clear-buffer))
  :config
  (require 'agent-shell-openai)
  (setq agent-shell-openai-codex-acp-command
        '("~/.local/share/mise/installs/node/25.9.0/bin/codex-acp")
        agent-shell-openai-codex-environment
        '("CODEX_HOME=/home/lance/dotfiles/.config/codex"
          "XDG_CONFIG_HOME=/home/lance/dotfiles/.config")))

(use-package agent-shell-extras
  :ensure (:type file :main "~/.emacs.d/lisp/agent-shell-extras.el" :files ("agent-shell-extras.el"))
  :hook (agent-shell-mode . +agent-shell-auto-rename-mode)
  :config
  (+modal-bind '+normal-mode-map 'agent-shell-mode-hook
               '(("Y" . +agent-shell-accept)
                 ("N" . +agent-shell-decline)))
  (setq agent-shell-permission-responder-function
        (+agent-shell-make-permission
         '((allow
            (read . ("~/.claude/" "~/.notifier/" "~/.bashrc" "~/bin/" "/tmp/"
                     "~/src/*" "~/code/*"))
            (write . ("~/.notifier/" "/tmp/" "/"))
            (execute . ("jj status" "jj diff*" "jj show*" "jj log*"
                        "rg" "rg *"
                        "emacsclient *"
                        "git checkout *" "git status *" "git diff *"
                        "git log*" "git show*" "git branch*"
                        "git remote -v *" "git ls-files*")))
           (ask
            (execute . ("sudo *" "ssh *" "git *")))))))
