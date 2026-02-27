;; -*- lexical-binding: t -*-
(use-package +meow
  :ensure nil
  :bind
  (:map meow-insert-state-keymap
        ("C-\\" . meow-sexp-mode)
        ("C-g" . #'+keyboard-quit-normal)
        ("M-F" . #'+mark-forward-word)
        ("M-B" . #'+mark-backward-word))
  (:map meow-motion-state-keymap
        ("q" . #'meow-quit)
        ("h" . #'meow-left)
        ("H" . #'meow-left-expand)
        ("l" . #'meow-right)
        ("SPC" . nil)
        ("L" . #'meow-right-expand)
        ("j" . #'meow-next)
        ("g" . #'meow-cancel-selection)
        ("[" . #'meow-beginning-of-thing)
        ("]" . #'meow-end-of-thing)
        ("k" . #'meow-prev)
        ("m" . #'meow-line)
        ("." . #'meow-bounds-of-thing)
        ("," . #'meow-inner-of-thing)
        ("y" . #'+meow-save))
  (:map meow-normal-state-keymap
        ("a" . #'beginning-of-visual-line)
        ("e" . #'end-of-visual-line)
        ("y" . #'+meow-yank-or-replace)
        ("-" . #'negative-argument)
        ("SPC" . nil)
        ("\\" . #'meow-sexp-mode)
        ("." . #'meow-bounds-of-thing)
        ("," . #'meow-inner-of-thing)
        ("v" . #'scroll-up-command)
        ("d" . #'kill-word)
        ("^" . #'repeat)
        ("z" . #'zap-up-to-char)
        ("M-d" . #'+delete-word-insert)
        ("b" . #'backward-word)
        ("l" . #'meow-right)
        ("D" . #'meow-backward-delete)
        ("f" . #'forward-word)
        ("F" . #'+mark-forward-word)
        ("s" . #'isearch-forward)
        ("@" . #'mark-sexp)
        ("r" . #'isearch-backward)
        ("g" . #'+meow-cancel-selection)
        ("/" . #'undo)
        ("G" . #'meow-grab)
        ("M-F" . #'+mark-forward-insert)
        ("M-B" . #'+mark-backward-insert)
        ("M-f" . #'+forward-word-insert)
        ("M-b" . #'+backward-word-insert)
        ("C-k" . #'+meow-change)
        ("h" . #'meow-left)
        ("H" . #'meow-left-expand)
        ("L" . #'meow-right-expand)
        ("SPC" . #'set-mark-command)
        ("i" . #'meow-insert)
        ("n" . #'next-line)
        ("N" . #'meow-next-expand)
        ("p" . #'previous-line)
        ("P" . #'meow-prev-expand)
        ("C-f" . #'+forward-char-insert)
        ("C-b" . #'+backward-char-insert)
        ("x" . #'+back-or-join)
        ("r" . #'+meow-replace-char)
        ("k" . #'kill-visual-line)
        ("o" . #'+open-line)
        ("=" . #'+expand-region)
        ("m" . #'back-to-indentation)
        (";" . #'avy-goto-char-2)
        ("/" . #'undo)
        ("t" . #'transpose-chars)
        ("T" . #'transpose-words)
        ("?" . #'undo-redo)
        ("w" . #'kill-region)
        ("W" . #'kill-ring-save)
        ("}" . #'forward-paragraph)
        ("{" . #'backward-paragraph)
        ("Y" . #'meow-sync-grab)
        ("<escape>" . #'keyboard-quit))
  (:map meow-sexp-state-keymap
        ("h" . #'backward-paragraph)
        ("l" . #'backward-paragraph)
        ("n" . #'forward-sentence)
        ("p" . #'backward-sentence)
        ("q" . #'+save-and-exit)
        ("f" . #'forward-sexp)
        ("b" . #'backward-sexp)
        ("i" . #'meow-insert-mode)
        ("o" . #'+expand-region)
        ("O" . #'+expand-region-2)
        ("g" . #'+meow-sexp-mode-quit)
        ("m" . #'meow-line)
        (";" . #'meow-reverse)
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

;;;###autoload
(defun +meow-visit (arg)
  "`meow-visit' but add the string to the isearch history."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (raw-text (meow--prompt-symbol-and-words
                    (if arg "Visit backward: " "Visit: ")
                    (point-min) (point-max) t))
         (text (replace-regexp-in-string "\\\\" "" (replace-regexp-in-string "\\\\\_<\\|\\\\\_>" "" raw-text)))
         (visit-point (meow--visit-point raw-text reverse)))
    (if visit-point
        (let* ((m (match-data))
               (marker-beg (car m))
               (marker-end (cadr m))
               (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
               (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (thread-first
            (meow--make-selection '(select . visit) beg end)
            (meow--select t))
          (meow--push-search raw-text)
          (meow--ensure-visible)
          (meow--highlight-regexp-in-buffer raw-text)
          (setq meow--dont-remove-overlay t)
          (+isearch-update-last-search text)
          (deactivate-mark))
      (message "Visit: %s failed" raw-text))))

;;;###autoload
(defun +meow-swap-grab-or-replace ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'meow-swap-grab)
    (call-interactively #'+meow-replace-char)))

;;;###autoload
(defun +meow-change (arg)
  (interactive "P")
  (cond (arg
         (beginning-of-visual-line)
         (kill-line arg)
         (save-excursion (newline))
         (meow-insert-mode)
         (indent-according-to-mode))
        ((and (region-active-p)
              (not (derived-mode-p 'yaml-mode))
              (eq (point) (save-excursion (back-to-indentation) (point)))
              (indent-according-to-mode)))
        ((region-active-p)
         (meow-change))
        (t
         (kill-line)
         (meow-insert-mode))))

;;;###autoload
(defun +meow-replace-char (char)
  (interactive "cChar:")
  (if (eq char ?\e) ;; escape to quit
      (keyboard-quit))
  (save-excursion
    (when (and (region-active-p) (eq (point) (region-end)))
      (backward-char))
    (progn
      (delete-char 1)
      (insert-char char))))

;;;###autoload
(defun +meow-cancel-selection ()
  (interactive)
  (when (featurep 'isearch)
    (save-excursion
      (lazy-highlight-cleanup)
      (isearch-exit)))
  (meow--cancel-second-selection)
  (meow-cancel-selection))

;;;###autoload
(defun +meow-pop-selection ()
  (interactive)
  (if (region-active-p)
      (meow-pop-selection)
    (set-mark-command 1)))

;;;###autoload
(defun +meow-open-below (arg)
  "Open a newline below and switch to INSERT state. The previous function just executed the RET
macro which made this send the query in gptel so I replaced it with newline."
  (interactive "p")
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'insert)
    (goto-char (line-end-position))
    (newline arg)
    (indent-according-to-mode)
    (setq-local meow--insert-pos (point))
    (when meow-select-on-open
      (setq-local meow--insert-activate-mark t))))

;;;###autoload
(defun +meow-yank-or-replace ()
  (interactive)
  (if (region-active-p)
      (meow-replace)
    (meow-yank)))

;;;###autoload
(defun +meow-save ()
  (interactive)
  (unless (region-active-p)
    (cond ((member (char-after (point)) +open-chars)
           (+expand-region 1))
          ((member (char-after (point)) +close-chars)
           (+expand-region 2))
          (t
           (meow-line 1))))
  (meow-save))

;;;###autoload
(defun +join-line ()
  (interactive)
  (join-line)
  (unless (derived-mode-p 'yaml-mode)
    (indent-for-tab-command)))

;;;###autoload
(defun +back-or-join ()
  (interactive)
  (cond ((eq last-command this-command)
         (+join-line))
        ((eq (point) (save-excursion (back-to-indentation) (point)))
         (+join-line))
        (t
         (back-to-indentation)))
  (deactivate-mark))

;;;###autoload
(defun +kill-line-or-region (arg)
  (interactive "P")
  (if (region-active-p)
      (meow-kill)
    (save-excursion
      (beginning-of-line)
      (kill-visual-line arg))))

;;;###autoload
(defun +backward-kill-sexp ()
  (interactive)
  (forward-char 1)
  (backward-kill-sexp))

;;;###autoload
(defun +save-and-exit ()
  (interactive)
  (save-buffer)
  (meow-normal-mode))

;;;###autoload
(defun +delete ()
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively #'delete-region)
        (indent-for-tab-command))
    (call-interactively #'backward-delete-char-untabify)))

;;;###autoload
(defun +meow-sexp-mode-quit ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (meow-normal-mode)))

;;;###autoload
(defun +meow-set-desired-state (state)
  (setq-local +meow-desired-state state))

;;;###autoload
(defun +meow-mode-get-state-advice (orig-func &rest args)
  (if +meow-desired-state
      +meow-desired-state
    (apply orig-func args)))

(advice-add 'meow--mode-get-state :around #'+meow-mode-get-state-advice)

;;;###autoload
(defun +meow-motion-mode ()
  (+meow-set-desired-state 'motion))

;;;###autoload
(defun +meow-mark-word (arg)
  (interactive "p")
  (if (and (region-active-p)
           (or (eq last-command this-command)
               (eq last-command #'+meow-mark-symbol)))
      (progn
        (require 'expand-region)
        (let ((er/try-expand-list
               '(er/mark-word
                 er/mark-symbol
                 er/mark-symbol-with-prefix
                 er/mark-next-accessor
                 er/mark-comment
                 er/mark-url
                 er/mark-email
                 er/mark-ts-node)))
          (with-point-at-region-beginning
           (er/expand-region 1))))
    (meow-mark-word 1)))

;;;###autoload
(defun +meow-mark-symbol (arg)
  (interactive "p")
  (if (and (region-active-p)
           (or (eq last-command this-command)
               (eq last-command #'+meow-mark-word)))
      (require 'expand-region
               (let ((er/try-expand-list
                      '(er/mark-word
                        er/mark-symbol
                        er/mark-symbol-with-prefix
                        er/mark-next-accessor
                        er/mark-method-call
                        er/mark-outside-quotes
                        er/mark-outside-pairs
                        er/mark-comment
                        er/mark-url
                        er/mark-email
                        er/mark-defun
                        er/mark-ts-node)))
                 (with-point-at-region-beginning
                  (er/expand-region 1))))
    (meow-mark-symbol 1)))

;;;###autoload
(defun +meow-append ()
  (interactive)
  (cond ((eolp)
         (deactivate-mark)
         (meow-insert))
        (t
         (meow-append))))

;;;###autoload
(defun +forward-word (N)
  (interactive "p")
  (meow-next-word N))

;;;###autoload
(defun +backward-word (N)
  (interactive "p")
  (meow-back-word N)
  (deactivate-mark))

;;;###autoload
(defun +meow-delete-char (N)
  (interactive "p")
  (cond ((not (region-active-p))
         (delete-char N))
        ((< (point) (mark))
         (delete-char N))
        (t
         (backward-char 1)
         (delete-char N))))

;;;###autoload
(defun +open-line (arg)
  (interactive "P")
  (cond (arg
         (if (eq (point) (save-excursion (end-of-line) (point)))
             (open-line arg)
           (save-excursion (beginning-of-line) (open-line arg))))
        ((eq (point) (save-excursion (end-of-line) (point)))
         (open-line (or arg 1))
         (next-line)
         (indent-according-to-mode)
         (meow-insert-mode))
        (t
         (beginning-of-visual-line)
         (open-line (or arg 1))
         (indent-according-to-mode)
         (meow-insert-mode))))

;;;###autoload
(defun +mark-forward-insert ()
  (interactive)
  (+mark-forward-word)
  (meow-insert-mode))

;;;###autoload
(defun +mark-backward-insert ()
  (interactive)
  (+mark-backward-word)
  (meow-insert-mode))

;;;###autoload
(defun +delete-word-insert (arg)
  (interactive "p")
  (kill-word arg)
  (meow-insert-mode))

;;;###autoload
(defun +forward-char-insert (arg)
  (interactive "p")
  (forward-char arg)
  (meow-insert-mode))

;;;###autoload
(defun +backward-char-insert (arg)
  (interactive "p")
  (backward-char arg)
  (meow-insert-mode))

;;;###autoload
(defun +keyboard-quit-normal ()
  (interactive)
  (meow-normal-mode)
  (+keyboard-quit))

;;;###autoload
(defun +forward-word-insert (arg)
  (interactive "p")
  (forward-word arg)
  (meow-insert-mode))

;;;###autoload
(defun +backward-word-insert (arg)
  (interactive "p")
  (backward-word arg)
  (meow-insert-mode))
