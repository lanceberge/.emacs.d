;;; -*- lexical-binding: t -*-
(use-package emacs ; initial keybindings of built-in functions
  :general
  (my-localleader-def
    "c" (general-simulate-key "C-c" :which-key "+Mode specific maps"))

  (my-leader-def
    "h"         (general-simulate-key "C-h"     :which-key "+Help")

    ;; Windows
    ";"       #'(shell-command                  :which-key "shell command")
    "w"         (general-simulate-key "C-w"     :which-key "+Windows") ; window command

    ;; Buffers
    "bs"      #'(save-buffer                    :which-key "write file")
    "bd"      #'(kill-this-buffer               :which-key "delete buffer")
    "bq"      #'(my/save-and-kill-buffer        :which-key "save and kill buffer")
    "b SPC d" #'(my/kill-window-and-buffer      :which-key "kill window and buffer")
    "br"        (lambda () (interactive)
                  (revert-buffer t t)           :which-key "revert-buffer")
    "bn"      #'(next-buffer                    :which-key "next buffer")
    "bp"      #'(previous-buffer                :which-key "previous buffer")

    ;; Eval elisp
    "er"      #'(eval-region                    :which-key "execute elisp region")
    "es"      #'(eval-last-sexp                 :which-key "execute elisp sexp")
    "ee"      #'(eval-expression                :which-key "evaluate elisp expression")
    "eb"      #'(eval-buffer                    :which-key "evaluate elisp buffer")
    "ef"      #'(eval-defun                     :which-key "evaluate elisp defun")
    "'"         (general-simulate-key "C-c '"   :which-key "open src block")

    ;; Find specific files
    "ofr" (lambda () (interactive)
            (tab-bar-switch-to-tab "org")
            (find-file "~/.emacs.d/README.org") :which-key "config")

    "oft" (lambda () (interactive)
            (tab-bar-switch-to-tab "org")
            (find-file "~/org/todo.org")        :which-key "todo")

    "ofc" (lambda () (interactive)
            (counsel-find-file "~/code/")       :which-key "todo"))

  ('normal
   "gs" #'(my/split-line-below         :which-key "split line below")
   "gS" #'(my/split-line-above         :which-key "split line above")
   "]b" #'(next-buffer                 :which-key "next buffer")
   "[b" #'(previous-buffer             :which-key "previous buffer")
   "[n"   (lambda () (interactive) (display-line-numbers-mode -1))
   "]n"   (lambda () (interactive) (display-line-numbers-mode +1))
   "g C-l" #'(end-of-visual-line       :which-key "end of visual line")
   "g C-h" #'(beginning-of-visual-line :which-key "beginning of visual line"))

  ;; TODO change for if mac vs linux
  ('(normal insert)
   "M-/" #'(comment-line :which-key "comment"))

  ('visual
   "M-/" #'(comment-dwim :which-key "comment"))


  ('(normal insert)
   :prefix "C-c"
   "SPC" (general-simulate-key "C-c C-c"))

  ('insert
   "C-y" #'yank ; otherwise is overridden by evil
   "C-e" #'end-of-line
   "C-a" #'beginning-of-line
   "C-w" (general-simulate-key "M-DEL"))

  ('insert '(prog-mode-map text-mode-map)
           "C-w" #'evil-delete-backward-word)
  :config
  (which-key-add-key-based-replacements
    "SPC br" "revert buffer"
    "SPC omi" "matlab inferior"
    "[n" "toggle line numbers off"
    "]n" "toggle line numbers on"))
