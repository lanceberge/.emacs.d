;;; -*- lexical-binding: t -*-
(use-package +modal
  :ensure nil
  :bind
  (:map +insert-mode-map
        ("C-\\" . #'+sexp-mode)
        ("C-g" . #'+keyboard-quit-normal)
        ("M-F" . #'+mark-forward-word)
        ("M-B" . #'+mark-backward-word)
        ("<escape>" . #'+normal-mode))
  (:map +motion-mode-map
        ("q" . #'quit-window)
        ("h" . #'backward-char)
        ("H" . #'+left-expand)
        ("l" . #'forward-char)
        ("SPC" . nil)
        ("L" . #'+right-expand)
        ("j" . #'next-line)
        ("g" . #'+keyboard-quit)
        ("y" . #'kill-ring-save))
  (:map +normal-mode-map
        ("C-a" . #'+beginning-of-line-insert)
        ("C-e" . #'+end-of-line-insert)
        ("<" . #'beginning-of-buffer)
        (">" . #'end-of-buffer)
        ("M-m" . #'+back-to-indentation-insert)
        ("C-w" . #'+change)
        ("a" . #'beginning-of-visual-line)
        ("e" . #'end-of-visual-line)
        ("y" . #'yank)
        ("-" . #'negative-argument)
        ("\\" . #'+sexp-mode)
        ("v" . #'scroll-up-command)
        ("d" . #'kill-word)
        ("^" . #'repeat)
        ("z" . #'zap-up-to-char)
        ("M-d" . #'+delete-word-insert)
        ("b" . #'backward-word)
        ("l" . #'forward-char)
        ("D" . #'backward-delete-char)
        ("f" . #'forward-word)
        ("F" . #'+mark-forward-word)
        ("s" . #'isearch-forward)
        ("@" . #'mark-sexp)
        ("r" . #'isearch-backward)
        ("g" . #'+keyboard-quit)
        ("/" . #'undo)
        ("M-F" . #'+mark-forward-insert)
        ("M-B" . #'+mark-backward-insert)
        ("M-f" . #'+forward-word-insert)
        ("M-b" . #'+backward-word-insert)
        ("C-k" . #'+kill-line-insert)
        ("h" . #'backward-char)
        ("H" . #'+left-expand)
        ("L" . #'+right-expand)
        ("SPC" . #'set-mark-command)
        ("i" . #'+insert-mode)
        ("n" . #'next-line)
        ("p" . #'previous-line)
        ("C-f" . #'+forward-char-insert)
        ("C-b" . #'+backward-char-insert)
        ("^" . #'delete-indentation)
        ("k" . #'kill-visual-line)
        ("o" . #'+open-line)
        ("=" . #'+expand-region)
        ("m" . #'back-to-indentation)
        (";" . #'avy-goto-char-2)
        ("t" . #'transpose-chars)
        ("T" . #'transpose-words)
        ("?" . #'undo-redo)
        ("w" . #'kill-region)
        ("W" . #'kill-ring-save)
        ("}" . #'forward-paragraph)
        ("{" . #'backward-paragraph)
        ("<escape>" . #'keyboard-quit)
        ("x TAB" . #'indent-rigidly)
        ("x SPC" . #'rectangle-mark-mode)
        ("xk" . #'kill-current-buffer)
        ("xh" . #'mark-whole-buffer)
        ("x0" . #'delete-window)
        ("x1" . #'delete-other-windows)
        ("x2" . #'split-window-below)
        ("x3" . #'split-window-right))
  (:map +sexp-mode-map
        ("h" . #'backward-paragraph)
        ("l" . #'backward-paragraph)
        ("n" . #'forward-sentence)
        ("p" . #'backward-sentence)
        ("q" . #'+save-and-exit)
        ("f" . #'forward-sexp)
        ("b" . #'backward-sexp)
        ("i" . #'+insert-mode)
        ("o" . #'+expand-region)
        ("O" . #'+expand-region-2)
        ("g" . #'+sexp-mode-quit)
        ("x" . #'+join-line)
        ("d" . #'kill-sexp)
        ("DEL" . #'backward-kill-sexp)
        ("a" . #'back-to-indentation)
        ("e" . #'end-of-line)
        ("s" . #'+kill-line-or-region)
        ("j" . #'next-line)
        ("k" . #'previous-line)
        ("u" . #'undo)
        ("r" . #'undo-redo)
        ("t" . #'transpose-sexps)
        ("T" . #'transpose-sentences)))

(use-package +keybindings
  :ensure nil
  :bind
  (:map +leader-map
        ("h" . #'help-command)
        ("d" . #'duplicate-line)
        ("q" . #'+server-edit)
        ("er" . #'+source-init-file)
        ;; ("[" . #'beginning-of-buffer)
        ;; ("]" . #'end-of-buffer)
        )
  (:map +leader2-map
        ("br" . #'rename-buffer))
  ;; escape sequences bound via +create-escape below
  (:map +normal-mode-map
        ("[ SPC" . #'+insert-newlines-above)
        ("] SPC" . #'+insert-newlines-below)))

;;;###autoload
(defun insert-newline-indent ()
  (interactive)
  (save-excursion
    (newline)
    (indent-for-tab-command)))

;;;###autoload
(defun +server-edit ()
  (interactive)
  (save-buffer)
  (server-edit))

;;;###autoload
(defun insert-newline-dwim ()
  (interactive)
  (if (region-active-p)
      (insert-newline-preserving-region)
    (progn
      (newline)
      (indent-for-tab-command))))

;;;###autoload
(defun insert-newline-above-dwim ()
  "split the line above at point"
  (interactive)
  (let ((deactivate-mark nil))
    (if (region-active-p)
        (progn
          (insert-newline-preserving-region)
          (save-excursion
            (next-line)
            (indent-for-tab-command)))
      (progn
        (insert-newline-dwim)
        (drag-stuff-up 1)
        (indent-for-tab-command)))))

;;;###autoload
(defun insert-newline-preserving-region ()
  "Insert a newline at point while preserving the region."
  (let* ((was-active (region-active-p))
         (old-point (point))
         (old-mark (and (region-active-p) (mark t)))
         (deactivate-mark nil)
         (point-at-end (eq old-point (region-end)))
         (region-len (abs (- old-point old-mark))))
    (insert "\n")
    (indent-according-to-mode)
    (when point-at-end
      (goto-char old-point)
      (set-mark (- (point) region-len))
      (activate-mark))))

;;;###autoload
(defun +project-shell-command ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))

;;;###autoload
(defun +source-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (looking-back ";" nil)
      (insert ";"))))

;;;###autoload
(defun +create-escape (keys)
  "Create and bind a two-key escape sequence KEYS in insert mode.
KEYS is a 2-char string like \"jk\". Pressing the first char waits
briefly for the second; if it arrives, exit to normal mode.
Otherwise insert the first char and handle the second normally."
  (let ((first (aref keys 0))
        (second (aref keys 1)))
    (define-key +insert-mode-map (string first)
                (let ((f first) (s second))
                  (lambda ()
                    (interactive)
                    (+escape--handle f s)))))
  keys)

(defun +escape--handle (first second)
  (let* ((cooldown 0.5)
         (char (read-char nil nil cooldown)))
    (if (null char)
        (insert-char first)
      (if (= char second)
          (progn
            (when (bound-and-true-p corfu--frame)
              (corfu-quit))
            (+normal-mode 1))
        (insert-char first)
        (let ((command (key-binding (vector char))))
          (cond
           ((null command))
           ((not (commandp command)))
           ((and (eq char ?\") (eq (char-after (point)) ?\"))
            (forward-char))
           ((memq command '(self-insert-command org-self-insert-command))
            (self-insert-command 1 char))
           (t
            (call-interactively command))))))))

(+create-escape "jk")
(+create-escape "kj")

(define-derived-mode keyfreq-show-mode special-mode "KeyFreq"
  "Major mode for displaying key frequency statistics."
  :group 'keyfreq
  (setq buffer-read-only t))

(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          mwheel-scroll
          mouse-drag-region
          next-line
          previous-line
          +escape
          forward-char
          backward-char
          undefined
          mouse-set-point
          org-self-insert-command))
  (keyfreq-autosave-mode)
  :bind
  (:map +leader-map
        ("SPC hk" . #'+keyfreq-show)))

;;;###autoload
(defun +keyfreq-show (&optional arg)
  (interactive "P")
  (let ((frequencies-buffer "*frequencies*"))
    (when (get-buffer frequencies-buffer)
      (kill-buffer frequencies-buffer))
    (keyfreq-show (if arg major-mode))
    (with-current-buffer frequencies-buffer
      (keyfreq-show-mode))
    (pop-to-buffer frequencies-buffer)))

(use-package keycast
  ;; :disabled t
  :hook (after-init . keycast-mode-line-mode)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '("<mouse-event>" "<mouse-movement>" "<mouse-2>" "<drag-mouse-1>" "<wheel-up>" "<wheel-down>" "<double-wheel-up>" "<double-wheel-down>" "<triple-wheel-up>" "<triple-wheel-down>" "<wheel-left>" "<wheel-right>" handle-select-window mouse-set-point  mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil nil))))

(use-package rect
  :ensure nil
  :bind
  (:map rectangle-mark-mode-map
        ([remap +smart-delete] . #'kill-rectangle)
        ("i" . #'string-insert-rectangle)
        ([remap +change] . #'replace-rectangle)))

;;;###autoload
(defun +rectangle-mode (&optional arg)
  (interactive "p")
  (rectangle-mark-mode)
  (forward-char)
  (next-line (- arg 1)))

;;;###autoload
(defun +insert-newlines-above (arg)
  (interactive "p")
  ;; save excursion didn't work if point is at line-beginning-position
  (let ((col (- (point) (line-beginning-position))))
    (beginning-of-line)
    (dotimes (_ arg)
      (newline))
    (forward-char col)))

;;;###autoload
(defun +insert-newlines-below (arg)
  (interactive "p")
  (save-excursion
    (end-of-line)
    (dotimes (_ arg)
      (newline))))
