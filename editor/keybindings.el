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
                (revert-buffer t t) :which-key "revert buffer")
    "bl" #'+switch-to-recent-file
    "bn" #'(next-buffer :which-key "next buffer")
    "bs" #'save-buffer
    "bp" #'(previous-buffer :which-key "previous buffer")

    ;; Eval elisp
    "es" #'(eval-last-sexp :which-key "execute elisp sexp")
    "ee" #'(eval-expression :which-key "evaluate elisp expression")
    "eb" #'(eval-buffer :which-key "evaluate elisp buffer")
    "ef" #'(eval-defun :which-key "evaluate elisp defun")

    ;; Find specific files
    "er" (defun +source-init-file () (interactive)
                (load-file "~/.emacs.d/init.el") :which-key "source init file"))


  ;; TODO
  ;; (my-leader-def
  ;;   :states 'visual
  ;;   "eb" #'(eval-region :which-key "execute elisp region"))

  ('meow-normal-state-keymap
   ;; TODO
   ;; "gs" #'(+split-line-below :which-key "split line below")
   ;; "gS" #'(+split-line-above :which-key "split line above")
   ;; "]b" #'(next-buffer :which-key "next buffer")
   ;; "[b" #'(previous-buffer :which-key "previous buffer")
   ;; "g C-l" #'(end-of-visual-line :which-key "end of visual line")
   ;; "g C-h" #'(beginning-of-visual-line :which-key "beginning of visual line")

   ;; TODO
   "C-u" #'scroll-down
   "C-d" #'scroll-up
   ;;    "m" nil
   ;;    "mm" #'(lambda () (interactive)
   ;; (bookmark-set (file-name-nondirectory buffer-file-name)))
   ;;   "md" #'(bookmark-delete-all :which-key "delete all bookmarks")
   "s-t" #'beginning-of-line)

  ('meow-normal-state-keymap
   "C-/" #'(comment-line :which-key "comment")
   "M-/" #'(comment-line :which-key "comment"))

  ('visual
   "C-/" #'(comment-dwim :which-key "comment")
   "M-/" #'(comment-dwim :which-key "comment")
   "q" #'apply-macro-to-region-lines)

  ('meow-normal-state-keymap
   "q" #'+start-or-end-macro)

  ('meow-insert-state-keymap
   "j" #'+escape)
  ('(normal insert)
   :prefix "C-c"
   "SPC" (general-simulate-key "C-c C-c"))

  ('insert
   "C-<backspace>" #'evil-delete-backward-word
   "M-<backspace>" #'evil-delete-backward-word)

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
  "Switch to the first recent file that is not the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (defun +switch-to-recent-file-helper (files)
      (if (not files)
          (message "No other recent files available")
        (let ((file (car files)))
          (if (and (file-exists-p file)
                   (not (eq (get-file-buffer file) current-buffer)))
              (find-file file)
            (+switch-to-recent-file-helper (cdr files))))))
    (+switch-to-recent-file-helper recentf-list)))

;; TODO revisit
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
