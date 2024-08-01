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
   "C-b" #'vterm--self-insert
   "C-c" #'vterm--self-insert)
  (my-leader-def
    "ost" (lambda () (interactive)
            (evil-window-split 10)
            (vterm))
    "ot" #'vterm)
  :config
  (cl-loop for num from 0 to 9 do
           (general-define-key :keymaps 'vterm-mode-map
                               :states '(normal insert)
                               (number-to-string num) #'vterm--self-insert))
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
