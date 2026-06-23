;;; -*- lexical-binding: t -*-
(use-package agent-shell
  :custom
  (agent-shell-show-config-icons nil)
  :bind
  (:map +leader2-map
        ("sc" . #'agent-shell-send-dwim))
  :config
  (setq agent-shell-openai-codex-acp-command
        '("~/.local/share/mise/installs/node/25.9.0/bin/codex-acp")
        agent-shell-openai-codex-environment
        '("CODEX_HOME=/home/lance/dotfiles/.config/codex"
          "XDG_CONFIG_HOME=/home/lance/dotfiles/.config")))

(use-package agent-shell-extensions
  :ensure (:type file :main "~/.emacs.d/packages/agent-shell-extensions.el")
  :defer 0.3
  :hook (agent-shell-mode . +agent-shell-auto-rename-mode)
  :bind
  (:map +leader-map
        ("a," . #'+consult-agent-shell-project-buffers))
  (:map +leader2-map
        ("st" . #'+agent-shell-toggle-dwim)
        ("sp" . #'+agent-shell-send-region-with-prompt))
  :config
  (setq agent-shell-permission-responder-function
        (+agent-shell-make-permission
         '((allow
            (read . ("~/.claude/" "~/.notifier/" "~/.bashrc" "~/bin/" "/tmp/"
                     "~/src/*" "~/code/*"))
            (write . ("~/.notifier/" "/tmp/" "/"))
            (execute . ("jj status" "jj diff*" "jj show*" "jj log*"
                        "emacsclient *"
                        "git checkout *" "git status *" "git diff *"
                        "git log*" "git show*" "git branch*"
                        "git remote -v *" "git ls-files*")))
           (ask
            (execute . ("sudo *" "ssh *" "git *")))))))

(use-package ace-link-agent-shell
  :ensure (:type file :main "~/.emacs.d/packages/ace-link-agent-shell.el")
  :hook (agent-shell-mode . +ace-link-agent-shell-mode)
  :bind
  (:map +ace-link-agent-shell-mode-map
        ("M-i" . #'ace-link)))
