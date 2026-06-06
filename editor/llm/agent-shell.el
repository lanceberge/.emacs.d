;;; -*- lexical-binding: t -*-
(use-package agent-shell
  :init
  (setq exec-path (append exec-path '("~/.local/share/mise/installs/node/25.9.0/lib/node_modules/@agentclientprotocol/")))
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
