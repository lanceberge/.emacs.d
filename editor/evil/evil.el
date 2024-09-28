;;; -*- lexical-binding: t -*-
(use-package evil ; vim bindings in emacs
  :demand t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-search-wrap t)
  (evil-want-fine-undo 'fine) ; save inserts as undo units more often
  (evil-ex-search-persistent-highlight nil)
  (evil-ex-substitute-highlight-all nil)
  (evil-ex-search-persist-highlight nil)
  :general
  ('normal ; navigate wrapped lines like normal lines, works great with relative line numbers
   [remap evil-next-line] #'evil-next-visual-line
   [remap evil-previous-line] #'evil-previous-visual-line
   "C-M-d" #'scroll-other-window
   "C-M-u" #'scroll-other-window-down
   "gm" (general-simulate-key "@@"))

  ('evil-ex-completion-map "C-g" 'abort-recursive-edit) ; quit on C-g

  ('(normal visual motion)
   "M-u" #'evil-scroll-up
   "M-d" #'evil-scroll-down
   ";" #'evil-ex ; switch ; and :
   "H" #'evil-first-non-blank
   "L" #'evil-end-of-line
   "]m" #'(+forward-global-mark :which-key "forward mark")
   "[m" #'(+backward-global-mark :which-key "backward mark")
   [M-right] #'(+forward-global-mark :which-key "forward mark")
   [M-left] #'(+backward-global-mark :which-key "backward mark"))

  ('normal 'evil-operator-state-map
           "?" #'evil-search-backward
           "/" #'evil-search-forward)

  (my-leader-def
    "bS" #'(evil-write-all :which-key "write all buffers")
    "bl" #'(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "bo" #'(evil-buffer-new :which-key "new buffer"))

  ('evil-window-map
   "SPC h" #'(evil-window-move-far-left :which-key "move window left")
   "SPC j" #'(evil-window-move-very-bottom :which-key "move window down")
   "SPC k" #'(evil-window-move-very-top :which-key "move window up")
   "SPC l" #'(evil-window-move-far-right :which-key "move window right")
   "d" #'(evil-quit :which-key "delete window")
   "q" #'(evil-save-modified-and-close :which-key "quit and save window")
   "SPC q" #'(save-buffers-kill-emacs :which-key "save buffers & quit emacs")
   "a" (defun +evil-window-increase () (interactive)
              (evil-window-increase-width 5) :which-key "increase size")
   "x" (defun +evil-window-decrease () (interactive)
              (evil-window-decrease-width 5) :which-key "decrease-size"))

  ('(normal insert)
   "C-l" #'(evil-ex-nohighlight :which-key "clear highlight"))
  :config
  (evil-mode))
