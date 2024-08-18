;;; -*- lexical-binding: t -*-
(use-package emacs ; initial keybindings of built-in functions
  :straight (:type built-in)
  :general
  (my-localleader-def
    "c" (general-simulate-key "C-c" :which-key "+Mode specific maps"))

  (my-leader-def
    "nf"      #'(make-frame                :which-key "New Frame")
    "h"       #'(help-command              :which-key "Help")
    ";"       #'(shell-command             :which-key "shell command")

    ;; Windows
    "w"       #'(evil-window-map           :which-key "Windows") ; window command

    ;; Buffers
    "bd"      #'(kill-current-buffer       :which-key "delete buffer")
    "bq"      #'(+save-and-kill-buffer     :which-key "save and kill buffer")
    "b SPC d" #'(+kill-window-and-buffer   :which-key "kill window and buffer")
    "br"        (defun +revert-buffer () (interactive)
                       (revert-buffer t t) :which-key "revert buffer")
    "bn"      #'(next-buffer               :which-key "next buffer")
    "bp"      #'(previous-buffer           :which-key "previous buffer")

    ;; Eval elisp
    "es"      #'(eval-last-sexp            :which-key "execute elisp sexp")
    "ee"      #'(eval-expression           :which-key "evaluate elisp expression")
    "eb"      #'(eval-buffer               :which-key "evaluate elisp buffer")
    "ef"      #'(eval-defun                :which-key "evaluate elisp defun")

    ;; Find specific files
    "er" (defun +source-init-file () (interactive)
                (load-file "~/.emacs.d/init.el") :which-key "source init file"))

  (my-leader-def
    :states 'visual
    "eb" #'(eval-region :which-key "execute elisp region"))

  ('normal
   "gs"    #'(+split-line-below        :which-key "split line below")
   "gS"    #'(+split-line-above        :which-key "split line above")
   "]b"    #'(next-buffer              :which-key "next buffer")
   "[b"    #'(previous-buffer          :which-key "previous buffer")
   "g C-l" #'(end-of-visual-line       :which-key "end of visual line")
   "g C-h" #'(beginning-of-visual-line :which-key "beginning of visual line"))

  ('normal
   "Y" (general-simulate-key "y$" :which-key "yank until end of line")

   "C-/" #'(comment-line :which-key "comment")
   "M-/" #'(comment-line :which-key "comment"))

  ('visual
   "C-/" #'(comment-dwim :which-key "comment")
   "M-/" #'(comment-dwim :which-key "comment"))


  ('(normal insert)
   :prefix "C-c"
   "SPC" (general-simulate-key "C-c C-c"))

  ('insert
   "C-e" #'end-of-line
   "C-a" #'beginning-of-line
   "C-<backspace>" #'evil-delete-backward-word
   "M-<backspace>" #'evil-delete-backward-word
   )
  :config
  (which-key-add-key-based-replacements
    "C-c r" "revert-buffer"))
