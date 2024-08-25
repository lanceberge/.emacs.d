;;; -*- lexical-binding: t -*-
(use-package terminal-here
  :general
  (my-leader-def
    "ot" #'terminal-here-launch :which-key "Launch terminal"))

(use-package tramp ; access remote files within emacs
  :defer t
  )

(use-package vterm
  :disabled t
  :general
  ('normal vterm-mode-map
           "N" #'vterm--self-insert
           "R" #'vterm--self-insert)
  ('(normal insert) vterm-mode-map
   "C-l" #'vterm--self-insert
   "C-b" #'vterm--self-insert
   "C-c" #'vterm--self-insert)
  (my-leader-def
    :states 'insert
    "C-c" #'vterm--self-insert)
  (my-leader-def
    "ov" #'(vterm :which-key "vterm"))
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
