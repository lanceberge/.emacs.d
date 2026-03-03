;;; -*- lexical-binding: t -*-
(bind-key "C-c" +leader-map)
(bind-key "c" +leader-map +normal-mode-map)
(bind-key "c" +leader-map +motion-mode-map)
(bind-key "c" +leader-map +sexp-mode-map)

(bind-key "x" +x-map +normal-mode-map)
(bind-key "x" +x-map +motion-mode-map)
(bind-key "x" +x-map +sexp-mode-map)

(bind-key "C-'" +leader2-map)
(bind-key "'" +leader2-map +normal-mode-map)
(bind-key "'" +leader2-map +motion-mode-map)
(bind-key "'" +leader2-map +sexp-mode-map)

(bind-key "[" mark-backward-keymap +normal-mode-map)
(bind-key "]" mark-forward-keymap +normal-mode-map)

(bind-key "[" mark-backward-keymap +sexp-mode-map)
(bind-key "]" mark-forward-keymap +sexp-mode-map)

(bind-key "`" +leader3-map +motion-mode-map)
(bind-key "`" +leader3-map +normal-mode-map)
(bind-key "`" +leader3-map +sexp-mode-map)

(use-package +modal
  :ensure nil
  :bind
  (:map +x-map
        ("x TAB" . #'indent-rigidly)
        ("x SPC" . #'rectangle-mark-mode)
        ("xk" . #'kill-current-buffer)
        ("xh" . #'mark-whole-buffer)
        ("x0" . #'delete-window)
        ("x1" . #'delete-other-windows)
        ("x2" . #'split-window-below)
        ("x3" . #'split-window-right))
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
        ("i" . #'+insert-mode)
        ;; Enter insert mode with default emacs keys
        ("C-a" . #'+beginning-of-line-insert)
        ("C-e" . #'+end-of-line-insert)
        ("M-m" . #'+back-to-indentation-insert)
        ("M-F" . #'+mark-forward-insert)
        ("M-B" . #'+mark-backward-insert)
        ("M-f" . #'+forward-word-insert)
        ("M-b" . #'+backward-word-insert)
        ("C-k" . #'+kill-line-insert)
        ("C-f" . #'+forward-char-insert)
        ("C-b" . #'+backward-char-insert)
        ("C-d" . #'+delete-char-insert)
        ("C-w" . #'+kill-region-insert)
        ("M-d" . #'+delete-word-insert)

        ("R" . #'+replace-char)
        ("<" . #'beginning-of-buffer)
        (">" . #'end-of-buffer)
        ("a" . #'beginning-of-visual-line)
        ("e" . #'end-of-visual-line)
        ("y" . #'yank)
        ("-" . #'negative-argument)
        ("\\" . #'+sexp-mode)
        ("v" . #'scroll-up-command)
        ("d" . #'kill-word)
        ("^" . #'repeat)
        ("z" . #'zap-up-to-char)
        ("b" . #'backward-word)
        ("l" . #'forward-char)
        ("D" . #'backward-delete-char)
        ("f" . #'forward-word)
        ("F" . #'+mark-forward-word)
        ("N" . #'+mark-forward-line)
        ("P" . #'+mark-backward-line)
        ("s" . #'isearch-forward)
        ("@" . #'mark-sexp)
        ("r" . #'isearch-backward)
        ("g" . #'+keyboard-quit)
        ("/" . #'undo)
        ("h" . #'backward-char)
        ("H" . #'+left-expand)
        ("L" . #'+right-expand)
        ("SPC" . #'set-mark-command)
        ("n" . #'next-line)
        ("p" . #'previous-line)
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
        ("<escape>" . #'keyboard-quit))
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
