;;; -*- lexical-binding: t -*-
(use-package emacs
  :ensure nil
  :general
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

    "wo" #'delete-other-windows
    "wd" #'delete-window
    "wj" #'other-window
    "wq" #'(lambda () (interactive) (save-buffer) (delete-window))
    "ws" #'split-window-below
    "wv" #'split-window-right

    ;; Buffers
    "bd" #'(kill-current-buffer :which-key "delete buffer")
    "bq" #'(+save-and-kill-buffer :which-key "save and kill buffer")
    "b SPC d" #'(+kill-window-and-buffer :which-key "kill window and buffer")
    "br" (defun +revert-buffer () (interactive)
                (revert-buffer t t)
                :which-key "revert buffer")
    "bl" #'+switch-to-recent-file
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

  ('(meow-normal-state-keymap meow-motion-state-keymap)
   "C-u" #'scroll-down
   "C-d" #'scroll-up)

  ('meow-normal-state-keymap
   "q" #'save-buffer
   "C" #'(lambda () (interactive) (kill-line) (meow-insert-mode))
   "d" #'(lambda () (interactive (delete-char 1))))

  ('meow-normal-state-keymap
   "c" #'(lambda () (interactive) (if (region-active-p) (meow-change)
                                    (progn  (delete-char 1) (meow-insert-mode)))))
  ('meow-normal-state-keymap
   "C-/" #'(comment-line :which-key "comment")
   "M-/" #'(comment-line :which-key "comment"))

  ('region-bindings-mode-map
   "C-/" #'(comment-dwim :which-key "comment")
   "M-/" #'(comment-dwim :which-key "comment")
   "q" #'apply-macro-to-region-lines)

  ;;  ('meow-normal-state-keymap
  ;;   "q" #'+start-or-end-macro)

  ('meow-insert-state-keymap
   "j" #'+escape)
  ('(normal insert)
   :prefix "C-c"
   "SPC" (general-simulate-key "C-c C-c"))

  ('(normal insert) '(php-mode-map c++-mode-map)
   "M-;" #'+append-semicolon))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  save-excursion
  (end-of-line)
  (unless (looking-back ";" nil)
    (Insert ";")))

;;;###autoload
(defun +switch-to-recent-file ()
  "Switch to the first recent file that is open in a buffer and not the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (defun +switch-to-recent-file-helper (files)
      (if (not files)
          (message "No other recent files open in buffers")
        (let ((file (car files)))
          (if (and (get-file-buffer file)
                   (not (eq (get-file-buffer file) current-buffer)))
              (switch-to-buffer (get-file-buffer file))
            (+switch-to-recent-file-helper (cdr files))))))
    (+switch-to-recent-file-helper recentf-list)))

;; TODO fix in org mode
;;;###autoload
(defun +escape (&optional count)
  (interactive)
  (let ((cooldown 0.5))
    (let ((char (read-char nil nil cooldown)))
      (if char
          (let* ((str (char-to-string char))
                 (command (key-binding str)))
            (if (= char ?k) (meow-insert-exit)
              (progn (insert-char ?j)
                     (if (eq command #'self-insert-command)
                         (insert-char char)
                       (funcall command)))))
        (insert-char ?j)))))
