;;; -*- lexical-binding: t -*-

;; initial keybindings, built-ins
(use-package emacs
  :ensure nil
  :general
  (my-localleader-def
    "c" (general-simulate-key "C-c" :which-key "+Mode specific maps"))

  (my-leader-def
    "nf" (defun +make-frame ()
           (interactive)
           (let ((frame (make-frame)))
             (when (and IS-LINUX (>= emacs-major-version 29))
               (set-frame-parameter frame 'undecorated t)))
           :which-key "New Frame")
    "h" #'(help-command :which-key "Help")
    ":" #'shell-command
    ";" #'(lambda ()
            (interactive)
            (let ((default-directory (project-root (project-current t))))
              (call-interactively #'shell-command)))

    ;; Windows
    "w" #'(evil-window-map :which-key "Windows") ; window command

    ;; Buffers
    "bd" #'(kill-current-buffer :which-key "delete buffer")
    "bq" #'(+save-and-kill-buffer :which-key "save and kill buffer")
    "b SPC d" #'(+kill-window-and-buffer :which-key "kill window and buffer")
    "br"        (defun +revert-buffer () (interactive)
                       (revert-buffer t t) :which-key "revert buffer")
    "bn" #'(next-buffer :which-key "next buffer")
    "bp" #'(previous-buffer :which-key "previous buffer")

    ;; Eval elisp
    "es" #'(eval-last-sexp :which-key "execute elisp sexp")
    "ee" #'(eval-expression :which-key "evaluate elisp expression")
    "eb" #'(eval-buffer :which-key "evaluate elisp buffer")
    "ef" #'(eval-defun :which-key "evaluate elisp defun")

    ;; Find specific files
    "er" (defun +source-init-file () (interactive)
                (load-file "~/.emacs.d/init.el") :which-key "source init file"))

  (my-leader-def
    :states 'visual
    "eb" #'(eval-region :which-key "execute elisp region"))

  ('normal
   "gs" #'(+split-line-below :which-key "split line below")
   "gS" #'(+split-line-above :which-key "split line above")
   "]b" #'(next-buffer :which-key "next buffer")
   "[b" #'(previous-buffer :which-key "previous buffer")
   "g C-l" #'(end-of-visual-line :which-key "end of visual line")
   "g C-h" #'(beginning-of-visual-line :which-key "beginning of visual line")

   "m" nil
   "mm" #'(lambda () (interactive)
            (bookmark-set (file-name-nondirectory buffer-file-name)))
   "md" #'(bookmark-delete-all :which-key "delete all bookmarks")
   "s-t" #'beginning-of-line)

  ('(normal insert)
   "C-/" #'(comment-line :which-key "comment")
   "M-/" #'(comment-line :which-key "comment"))

  ('visual
   "C-/" #'(comment-dwim :which-key "comment")
   "M-/" #'(comment-dwim :which-key "comment")
   "q" #'apply-macro-to-region-lines)

  ('normal
   "q" #'+start-or-end-macro)

  ('(normal insert)
   :prefix "C-c"
   "SPC" (general-simulate-key "C-c C-c"))

  ('insert
   "C-e" #'end-of-line
   "C-a" #'beginning-of-line
   "C-<backspace>" #'evil-delete-backward-word
   "M-<backspace>" #'evil-delete-backward-word)

  ('(normal insert)
   '(php-mode-map
     c++-mode-map)
   "M-;" #'+append-semicolon))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (looking-back ";" nil)
      (insert ";"))))

(defvar +macro-recording nil)

;;;###autoload
(defun +start-or-end-macro (arg)
  (interactive "p")
  (if +macro-recording
      (kmacro-end-macro arg)
    (kmacro-start-macro arg))
  (setq +macro-recording (not +macro-recording)))
