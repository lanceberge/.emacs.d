;;; -*- lexical-binding: t -*-
(use-package terminal-here
  :general
  (my-leader-def
    "ot" #'terminal-here-launch :which-key "Launch terminal"))

(use-package tramp ; access remote files within emacs
  :disabled t
  :straight (:type built-in)
  :general
  (my-localleader-def
    "tr" #'(+tramp/ssh-rlogin   :which-key "rlogin")
    "tc" #'(+tramp/ssh-cascades :which-key "cascades")
    "tp" #'(+tramp/ssh-pascal   :which-key "pascal")))

(use-package vterm
  :general
  ('(normal insert) vterm-mode-map
   "C-l" #'vterm--self-insert
   "C-c" #'vterm--self-insert)
  (my-leader-def
    "ost" (lambda () (interactive)
            (evil-window-split 12)
            (vterm))
    "ot" #'vterm)
  :config
  (evil-collection-init 'vterm)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

;;;###autoload
(defun +tramp/ssh-rlogin ()
  "ssh into rlogin through tramp"
  (interactive)
  (find-file "/sshx:lancebergeron@rlogin.cs.vt.edu:~/"))

;;;###autoload
(defun +tramp/ssh-cascades ()
  "ssh into cascades through tramp"
  (interactive)
  (find-file "/sshx:lancebergeron@cascades1.arc.vt.edu:~/"))

;;;###autoload
(defun +tramp/ssh-pascal ()
  "ssh into pascal through tramp"
  (interactive)
  (find-file "/sshx:cmda02@pascal.math.vt.edu|sshx:cmda02@node02:~/"))
