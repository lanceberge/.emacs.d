;;; -*- lexical-binding: t -*-
(bind-key "C-c" +leader-map)
(bind-key "c" +leader-map +normal-mode-map)
(bind-key "c" +leader-map +motion-mode-map)
(bind-key "c" +leader-map +sexp-mode-map)

(bind-key "x" ctl-x-map +normal-mode-map)
(bind-key "x" ctl-x-map +motion-mode-map)
(bind-key "x" ctl-x-map +sexp-mode-map)

(bind-key "h" help-map +leader-map)
;; use the default M-s everywhere else
(bind-key "s" search-map +normal-mode-map)

(bind-key "C-'" +leader2-map)
(bind-key "'" +leader2-map +normal-mode-map)
(bind-key "'" +leader2-map +motion-mode-map)
(bind-key "'" +leader2-map +sexp-mode-map)

(bind-key "[" mark-backward-keymap +sexp-mode-map)
(bind-key "]" mark-forward-keymap +sexp-mode-map)

(bind-key "`" +leader3-map +motion-mode-map)
(bind-key "`" +leader3-map +normal-mode-map)
(bind-key "`" +leader3-map +sexp-mode-map)

(use-package +keybindings
  :ensure nil
  :bind
  (:map +leader-map
        ;; ("h" . #'help-command)
        ("d" . #'duplicate-dwim)
        ("ri" . #'+source-init-file))
  (:map +normal-mode-map
        ("[ SPC" . #'+insert-newlines-above)
        ("] SPC" . #'+insert-newlines-below)))

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

(use-package rect
  :ensure nil
  :bind
  (:map rectangle-mark-mode-map
        ("w" . #'kill-rectangle)
        ("i" . #'string-insert-rectangle)
        ([remap +change] . #'replace-rectangle)))

;;;###autoload
(defun +key-chord-define-keymap (keymap keys prefix-keymap)
  "In KEYMAP, bind key chord KEYS as a prefix for PREFIX-KEYMAP."
  (unless (keymapp prefix-keymap)
    (user-error "Expected a keymap, got %S" prefix-keymap))
  (key-chord-define keymap keys prefix-keymap))

(use-package key-chord
  :custom
  (key-chord-two-keys-delay 0.2)
  :hook
  (emacs-startup . key-chord-mode)
  :config
  (key-chord-define +insert-mode-map "jk" #'+normal-mode))
