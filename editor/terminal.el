(use-package terminal-here
  :general
  (my-leader-def
    "ot" #'terminal-here-launch :which-key "Launch terminal"))

(use-package tramp ; access remote files within emacs
  :disabled t
  :straight (:type built-in)
  :custom
  (tramp-autosave-directory    (expand-file-name "tramp/auto-save/" my/etc-dir))
  (tramp-persistency-file-name (expand-file-name "tramp/persistency.el" my/etc-dir))
  :general
  (my-localleader-def
    "tr" #'(+tramp/ssh-rlogin   :which-key "rlogin")
    "tc" #'(+tramp/ssh-cascades :which-key "cascades")
    "tp" #'(+tramp/ssh-pascal   :which-key "pascal")))

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
