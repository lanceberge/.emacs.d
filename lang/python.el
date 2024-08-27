;;; -*- lexical-binding: t -*-
(use-package python-mode
  :defer t)

(use-package ein ; work with ipynb files
  :mode ("\\.ipynb\\'" . ein:ipynb-mode)
  :hook (ipynb-mode . display-line-numbers-mode)
  :custom
  (ein:output-area-inlined-images t)
  :general
  ('ein:notebook-mode-map
   "C-j" #'ein:worksheet-goto-next-input-km
   "C-k" #'ein:worksheet-goto-prev-input-km
   "M-:" #'ein:worksheet-execute-cell-and-insert-below-km
   "M-j" #'ein:worksheet-move-cell-down-km
   "M-k" #'ein:worksheet-move-cell-up-km
   "M-:" #'ein:worksheet-insert-cell-above-km
   "M-;" #'ein:worksheet-insert-cell-below-km
   "C-;" #'ein:worksheet-execute-cell-km
   [remap evil-write] #'ein:notebook-save-notebook-command)

  ('normal ein:notebook-mode-map
           :prefix "C-c"
           "d"   #'(ein:worksheet-delete-cell             :which-key "delete cell")
           "w"   #'(ein:notebook-save-notebook-command    :which-key "save-notebook")
           "k"   #'(ein:worksheet-kill-cell-km            :which-key "kill cell")
           "c"   #'(ein:worksheet-clear-all-output-km     :which-key "clear output")
           "q"   #'(ein:notebook-close                    :which-key "close")
           "SPC" #'(ein:worksheet-execute-all-cells       :which-key "run all cells")
           "r"   #'(ein:notebook-restart-session-command) :which-key "restart")

  (my-leader-def
    "ei"   '(:ignore t :which-key "Ein")
    "eir" #'(ein:run   :which-key "run")
    "eis" #'(ein:stop  :which-key "stop")))
