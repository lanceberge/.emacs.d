;;; -*- lexical-binding: t -*-
(use-package evil-collection ; evil bindings for many modes
  :defer t
  :custom
  (evil-collection-want-unimpaired-p t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-mode-list
   '(minibuffer
     dired
     eshell
     ediff
     (pdf pdf-tools)
     magit))
  :general
  ('normal 'org-mode-map
           [remap evil-collection-unimpaired-move-text-up]   #'(org-metaup   :which-key "move up")
           [remap evil-collection-unimpaired-move-text-down] #'(org-metadown :which-key "move down")
           )
  )

(use-package evil-snipe ; 2 character searches with s (ala vim-sneak)
  :hook (pre-command . evil-snipe-mode)
  :custom
  (evil-snipe-show-prompt nil)
  (evil-snipe-skip-leading-whitespace nil)
  :general
  ('normal
   [remap evil-find-char] #'evil-snipe-f
   [remap evil-find-char-backward] #'evil-snipe-F)

  ('motion
   ":"   #'(evil-snipe-repeat         :which-key "repeat last search")
   "M-," #'(evil-snipe-repeat-reverse :which-key "repeat last search backwards")))

(use-package evil-surround ; s as an operator for surrounding
  :hook (pre-command . evil-surround-mode))

(use-package evil-embrace ; custom surround pairs
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook
            (lambda ()
              (dolist (var '((?s "#+BEGIN_SRC" . "#+END_SRC")
                             (?$ "$" . "$")))
                (embrace-add-pair (car var) (cadr var) (cddr var))))))

(use-package evil-nerd-commenter ; comment lines like in tpope's commentary
  :general
  ('normal
   "gc" #'(evilnc-comment-operator          :which-key "comment")
   "gy" #'(evilnc-copy-and-comment-operator :which-key "copy and comment")))

(use-package evil-numbers ; increment/decrement numbers w/ vim keys
  :general
  ('normal override
           "C-a" #'(evil-numbers/inc-at-pt :which-key "increment number")
           "C-x" #'(evil-numbers/dec-at-pt :which-key "decrement number")))

(use-package evil-lion ; gl as an operator to left-align, gL to right-align
  :hook
  ((prog-mode text-mode) . evil-lion-mode)
  :general
  ('normal
   [remap evil-lion-right] #'(+align-keybindings :which-key "align keybindings")
   :config
   (defun +align-keybindings (count)
     (interactive "P")
     (save-excursion
       (beginning-of-line)
       (let ((start-pos (line-beginning-position))
             (end-pos (progn (forward-line (1+ count)) (point))))
         (evil-lion-left 1 start-pos end-pos ?\")
         (evil-lion-left 1 start-pos end-pos ?\#)
         (evil-lion-left 1 start-pos end-pos ?\()
         (evil-lion-left 1 start-pos end-pos ?\:)
         )
       (format-all-buffer)
       ))))

(use-package evil-matchit ; navigate matching blocks of code with %
  :hook (find-file . evil-matchit-mode)
  :general
  ('motion
   "%" #'(evilmi-jump-items :which-key "jump to matching pair")))

(use-package evil-exchange ; exchange text selected with gx
  :general
  ('(normal visual)
   "gx" #'(evil-exchange        :which-key "exchange operator")
   "gX" #'(evil-exchange-cancel :which-key "cancel exchange")))

(use-package evil-textobj-anyblock
  :general
  ('evil-inner-text-objects-map "c" #'(evil-textobj-anyblock-inner-block :which-key "nearest text object"))
  ('evil-outer-text-objects-map "c" #'(evil-textobj-anyblock-a-block     :which-key "nearest text object")))

(use-package evil-args ; argument text object: ex. arg1,ar|g2,arg2 - can delete with daa
  :general
  ('evil-inner-text-objects-map "a" #'(evil-inner-arg :which-key "inner arg"))
  ('evil-outer-text-objects-map "a" #'(evil-outer-arg :which-key "outer arg")))

(use-package evil-indent-plus ; indent level text object
  :general
  ('evil-inner-text-objects-map
   "i" #'(evil-indent-plus-i-indent         :which-key "indent level")
   "I" #'(evil-indent-plus-i-indent-up      :which-key "indent level and up")
   "J" #'(evil-indent-plus-i-indent-up-down :which-key "indent level up and down"))

  ('evil-outer-text-objects-map
   "i" #'(evil-indent-plus-a-indent         :which-key "indent level")
   "I" #'(evil-indent-plus-a-indent-up      :which-key "indent level and up")
   "J" #'(evil-indent-plus-a-indent-up-down :which-key "indent level up and down")))

(use-package evil-escape ; jk to leave insert mode
  :hook
  (prog-mode . evil-escape-mode)
  (text-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.25)
  (evil-escape-excluded-major-modes '(org-agenda-mode))
  (evil-escape-excluded-states '(normal visual emacs))
  :general
  ('(insert visual)
   "<escape>" #'evil-escape))

(use-package titlecase ; title case a line
  :general
  ('normal
   "g^" #'(titlecase-line :which-key "titlecase line"))

  ('visual
   "g^" #'titlecase-region :which-key "titlecase region"))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "'")
  )
