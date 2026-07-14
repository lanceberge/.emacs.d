;;; -*- lexical-binding: t -*-
(bind-key "SPC" +leader-map +normal-mode-map)
(bind-key "SPC" +leader-map +motion-mode-map)
(bind-key "SPC" +leader-map +sexp-mode-map)
(bind-key "C-c" +leader-map +insert-mode-map)

(bind-key "x" ctl-x-map +normal-mode-map)
(bind-key "x" ctl-x-map +motion-mode-map)
(bind-key "x" ctl-x-map +sexp-mode-map)

(bind-key "h" help-map +leader-map)
;; use the default M-s everywhere else
(bind-key "s" search-map +normal-mode-map)
(bind-key "a" +llm-map +leader-map)

(bind-key "[" +backward-map +normal-mode-map)
(bind-key "]" +forward-map +normal-mode-map)
(bind-key "[" +backward-map +motion-mode-map)
(bind-key "]" +forward-map +motion-mode-map)

;; (bind-key "C-'" +leader2-map)
;; (bind-key "'" +leader2-map +normal-mode-map)
;; (bind-key "'" +leader2-map +motion-mode-map)
;; (bind-key "'" +leader2-map +sexp-mode-map)

;;;###autoload
(defun +keybindings-C-c ()
  (interactive)
  (setq unread-command-events
        (append (listify-key-sequence (kbd "C-c"))
                unread-command-events)))

(use-package +keybindings
  :ensure nil
  :bind
  ("M-T" . #'transpose-paragraphs)
  ("M-[" . #'+pop-to-mark)
  ("M-]" . #'+unpop-to-mark)
  (:map ctl-x-map
        ("TAB" . #'+indent-rigidly-dwim)
        ("f" . #'find-file)
        ("j" . #'dired-jump))
  (:map +leader-map
        ("bs" . #'+scratch-buffer)
        ("d" . #'duplicate-dwim)
        ("ri" . #'+source-init-file)
        ("c" . #'+keybindings-C-c))
  (:map prog-mode-map
        ("C-g" . #'+keyboard-quit))
  (:map text-mode-map
        ("C-g" . #'+keyboard-quit))
  (:map +insert-mode-map
        ("C-\\" . #'+sexp-mode)
        ("C-g" . #'+keyboard-quit-normal)
        ;; ("<escape>" . #'+normal-mode)
        ("C-x C-s" . #'+save-buffer-normal))
  (:map +motion-mode-map
        ("q" . #'quit-window)
        ("g" . #'+keyboard-quit)
        ("{" . #'backward-paragraph)
        ("}" . #'forward-paragraph))
  (:map +normal-mode-map
        ("i" . #'+insert-mode)

        ("RET" . #'newline)
        ("S-<return>" . #'insert-newline-above-dwim)
        ("C-g" . #'+keyboard-quit)

        ("M-{" . #'+backward-global-mark)
        ("M-}" . #'+forward-global-mark)

        ;; Enter insert mode with default emacs keys
        ("C-a" . #'+modal-beginning-of-visual-line-insert)
        ("C-e" . #'+modal-end-of-line-insert)
        ("M-m" . #'+modal-back-to-indentation-insert)
        ("M-f" . #'+modal-forward-word-insert)
        ("M-b" . #'+modal-backward-word-insert)
        ("C-k" . #'+modal-kill-line-insert)
        ("C-M-k" . #'+modal-kill-sexp-insert)
        ("C-f" . #'+modal-end-of-region-or-forward-char-insert)
        ("C-b" . #'+modal-start-of-region-or-backward-char-insert)
        ("C-d" . #'+modal-delete-char-insert)
        ("C-w" . #'+modal-kill-region-insert)
        ("M-d" . #'+modal-kill-word-insert)
        ("M-<backspace>" . #'+modal-backward-kill-word-insert)
        ("C-<backspace>" . #'+modal-backward-delete-char-insert)
        ("C-n" . #'+modal-next-line-insert)
        ("C-p" . #'+modal-previous-line-insert)
        ("M->" . #'+modal-end-of-buffer-insert)
        ("M-z" . #'+modal-zap-up-to-char-insert)

        ("X" . #'exchange-point-and-mark)
        ("s-u" . #'+window-revert-buffer)
        ("S-<backspace>" . #'backward-kill-word)
        ("R" . #'+replace-char)
        ("<" . #'beginning-of-buffer)
        (">" . #'end-of-buffer)
        ("a" . #'beginning-of-visual-line)
        ("e" . #'end-of-visual-line)
        ("y" . #'yank)
        ("-" . #'negative-argument)
        ("v" . #'scroll-up-command)
        ("d" . #'kill-word)
        ("z" . #'zap-up-to-char)
        ("b" . #'backward-word)
        ("l" . #'forward-char)
        ("d" . #'delete-char)
        ("f" . #'forward-word)

        ("@" . #'mark-sexp)
        ;; ("r" . #'isearch-backward)
        ("g" . #'+keyboard-quit)
        ("/" . #'undo)
        ("h" . #'backward-char)
        ("H" . #'+left-expand)
        ("L" . #'+right-expand)
        ;; ("SPC" . #'set-mark-command)
        ("n" . #'next-line)
        ("M" . #'+mark-whole-lines)
        ("E" . #'+modal-mark-end-of-line)
        ("A" . #'+modal-mark-beginning-of-line)
        ("p" . #'previous-line)
        ("^" . #'delete-indentation)
        ("k" . #'+kill-line-dwim)
        ("o" . #'+open-line)
        ("=" . #'+expand-region)
        ("m" . #'back-to-indentation)
        (";" . #'avy-goto-char-2)
        ("t" . #'transpose-chars)
        ("T" . #'transpose-words)
        ("?" . #'undo-redo)
        ("D" . #'kill-word)
        ("w" . #'kill-region)
        ("}" . #'forward-paragraph)
        ("{" . #'backward-paragraph)
        ;; ("<escape>" . #'keyboard-quit)
        ("\\f" . #'forward-sexp)
        ("\\b" . #'backward-sexp)
        ("\\k" . #'kill-sexp)
        ("\\ <backspace>" . #'backward-kill-sexp)
        ("\\t" . #'transpose-sexps))
  (:map +backward-map
        ("SPC" . #'+insert-newlines-above))
  (:map +forward-map
        ("SPC" . #'+insert-newlines-below)))

(define-derived-mode keyfreq-show-mode special-mode "KeyFreq"
  "Major mode for displaying key frequency statistics."
  :group 'keyfreq
  (setq buffer-read-only t))

(use-package keyfreq
  :hook (emacs-startup . keyfreq-mode)
  :commands (+keyfreq-show)
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
  (keyfreq-autosave-mode))

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
  :hook (emacs-startup . keycast-mode-line-mode)
  :custom
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-insert-after 'mode-line-format-right-align)
  (keycast-mode-line-format "%k%c%r")
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '("<mouse-event>" "<mouse-movement>" "<mouse-2>" "<drag-mouse-1>" "<wheel-up>" "<wheel-down>" "<double-wheel-up>" "<double-wheel-down>" "<triple-wheel-up>" "<triple-wheel-down>" "<wheel-left>" "<wheel-right>" handle-select-window mouse-set-point  mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil nil))))

(use-package key-chord
  :custom
  (key-chord-two-keys-delay 0.2)
  :hook
  (emacs-startup . key-chord-mode)
  :config
  (key-chord-define +insert-mode-map "jk" #'+normal-mode))
