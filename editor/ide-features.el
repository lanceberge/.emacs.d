;;; -*- lexical-binding: t -*-
(use-package eglot
  :defer 1.4
  :commands (eglot-shutdown)
  :hook
  ((go-mode
    java-mode java-ts-mode
    c++-mode c++-ts-mode
    elixir-mode elixir-ts-mode
    typescript-mode typescript-ts-mode
    nix-mode
    tsx-ts-mode
    js2-mode
    python-base-mode
    php-mode
    rust-mode rust-ts-mode
    sh-base-mode
    scala-mode
    svelte-mode
    vue-mode) . eglot-ensure)
  :custom
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  :bind
  (:map +leader-map
        ("te" . #'+eglot-toggle-debug))
  (:map +leader2-map
        ("er" . #'eglot-reconnect))
  :config
  (setq eglot-sync-connect 3) ; Wait longer for connection

  ;; save buffers after eglot-renaming
  (advice-add 'eglot-rename :after
              (lambda (&rest _)
                (save-some-buffers t)))

  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (let ((elixir-lsp-path (if (eq system-type 'darwin)
                             "~/.local/bin/expert_darwin_arm64"
                           "~/.local/bin/expert_linux_amd64")))
    (add-to-list 'eglot-server-programs
                 (list '(elixir-mode elixir-ts-mode heex-ts-mode)
                       elixir-lsp-path "--stdio")))

  (custom-set-faces
   '(eglot-highlight-symbol-face ((t (:inherit nil)))))

  (add-to-list 'eglot-server-programs '(python-ts-mode . ("/opt/homebrew/bin/pyright-langserver" "--stdio")))

  ;; https://github.com/joaotavora/eglot/discussions/1184
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))

;;;###autoload
(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
                    "lib"
                    (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
    `(:typescript (:tsdk ,tsdk-path)
                  :vue (:hybridMode :json-false))))

(use-package eldoc
  :ensure nil
  :hook (emacs-lisp-mode . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-display-functions '(eldoc-display-in-echo-area))
  (eldoc-idle-delay 1)
  ;; https://www.reddit.com/r/emacs/comments/1lbo5jy/comment/myig4p7/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (eglot-code-action-indications '(eldoc-hint)))

;;;###autoload
(defun +eldoc-help ()
  "Show eldoc info for current symbol and restore cursor position."
  (interactive)
  (let ((win (selected-window)))
    (with-selected-window win
      (call-interactively #'eldoc-print-current-symbol-info)
      (run-with-timer 0.1 nil
                      (lambda ()
                        (select-window win))))))

(unless (version<= emacs-version "29.1")
  (use-package dape
    :after hydra
    :config
    (remove-hook 'dape-start-hook 'dape-info)
    (remove-hook 'dape-start-hook 'dape-repl)
    (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
              "
  ^Stepping^          ^Breakpoints^               ^Info
  ^^^^^^^^-----------------------------------------------------------
  _d_: init           _bb_: Toggle (add/remove)   _si_: Info
  _n_: Next           _bd_: Delete                _sm_: Memory
  _i_: Step in        _bD_: Delete all            _ss_: Select Stack
  _o_: Step out       _bl_: Set log message       _R_: Repl
  _c_: Continue
  _r_: Restart
  _Q_: Disconnect
  "
              ("d" dape)
              ("n" dape-next)
              ("i" dape-step-in)
              ("o" dape-step-out)
              ("c" dape-continue)
              ("r" dape-restart)
              ("ba" dape-breakpoint-toggle)
              ("bb" dape-breakpoint-toggle)
              ("be" dape-breakpoint-expression)
              ("bd" dape-breakpoint-remove-at-point)
              ("bD" dape-breakpoint-remove-all)
              ("bl" dape-breakpoint-log)
              ("si" dape-info)
              ("sm" dape-read-memory)
              ("ss" dape-select-stack)
              ("R"  dape-repl)
              ("q" nil "quit" :color blue)
              ("Q" dape-kill :color red))))

(use-package xref
  :commands (xref-find-references xref-auto-jump-first-definition)
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package jsonrpc
  :config
  (defvar +eglot--jsonrpc-log-event-default nil
    "Default function used for `jsonrpc--log-event'.")
  (when (or (null +eglot--jsonrpc-log-event-default)
            (symbolp +eglot--jsonrpc-log-event-default))
    (setq +eglot--jsonrpc-log-event-default
          (symbol-function 'jsonrpc--log-event)))

  (fset 'jsonrpc--log-event #'ignore))

;;;###autoload
(defun +eglot-toggle-debug ()
  "Toggle Eglot JSONRPC event logging."
  (interactive)
  (if (eq (symbol-function 'jsonrpc--log-event) #'ignore)
      (progn
        (fset 'jsonrpc--log-event +eglot--jsonrpc-log-event-default)
        (setq eglot-events-buffer-config '(:size 2000000 :format full))
        (message "Eglot JSONRPC debug logging enabled"))
    (fset 'jsonrpc--log-event #'ignore)
    (setq eglot-events-buffer-config '(:size 0 :format short))
    (message "Eglot JSONRPC debug logging disabled")))

(use-package flymake
  :custom
  (flymake-no-changes-timeout 5)
  (flymake-show-diagnostics-at-end-of-line nil)
  :bind
  (:repeat-map flymake-repeat-map
               ("n" . #'flymake-goto-next-error)
               ("p" . #'flymake-goto-prev-error))
  (:map +normal-mode-map
        ("]e" . #'flymake-goto-next-error)
        ("[e" . #'flymake-goto-prev-error)))

;;;###autoload
(defun +modal-flyover-on ()
  "Enable `flyover-mode' for the current buffer."
  (flyover-mode 1))

;;;###autoload
(defun +modal-flyover-off ()
  "Disable `flyover-mode' for the current buffer."
  (flyover-mode -1))

(define-minor-mode +modal-flyover-mode
  "Toggle flyover in normal mode."
  :lighter ""
  (if +modal-flyover-mode
      (progn
        (add-hook '+normal-mode-hook '+modal-flyover-on nil t)
        (add-hook '+insert-mode-hook '+modal-flyover-off nil t))
    (remove-hook '+normal-mode-hook '+modal-flyover-on t)
    (remove-hook '+insert-mode-hook '+modal-flyover-off t)))

;;;###autoload
(defun +flyover-configure-next-line-overlay (overlay face msg beg error)
  "Configure Flyover OVERLAY before the next line instead of on the previous newline."
  (condition-case configure-err
      (when (overlayp overlay)
        (flyover--setup-basic-overlay-properties overlay error t)
        (let* ((components (flyover--create-overlay-display-components face error msg))
               (final-string (flyover--build-final-overlay-string components error msg)))
          (overlay-put overlay 'before-string
                       (propertize final-string
                                   'help-echo msg
                                   'rear-nonsticky t
                                   'cursor-sensor-functions nil))))
    (error
     (flyover--handle-error 'overlay-configuration configure-err
                            "configure-next-line-overlay" (format "beg=%S" beg)))))

;;;###autoload
(defun +flyover-create-next-line-overlay (original region level msg &optional error)
  "Create Flyover's below-line overlay without replacing the current line's newline."
  (if flyover-show-at-eol
      (funcall original region level msg error)
    (let ((overlay nil))
      (condition-case ov-err
          (let ((beg (car region))
                (end (cdr region)))
            (save-excursion
              (goto-char (min end (point-max)))
              (forward-line flyover-line-position-offset)
              (let ((overlay-pos (line-beginning-position))
                    (face (flyover--get-face level)))
                (when (and (numberp beg)
                           (numberp end)
                           (numberp overlay-pos)
                           (> beg 0)
                           (> end 0)
                           (> overlay-pos 0)
                           (<= beg (point-max))
                           (<= end (point-max))
                           (<= overlay-pos (point-max)))
                  (setq overlay (make-overlay overlay-pos overlay-pos))
                  (overlay-put overlay 'flyover-beg beg)
                  (+flyover-configure-next-line-overlay overlay face msg beg error)))))
        (error
         (flyover--handle-error 'overlay-creation ov-err "create-next-line-overlay"
                                (format "region=%S level=%S" region level))))
      overlay)))

(use-package flyover
  :hook ((flymake-mode . +modal-flyover-mode))
  :custom
  ;; Checker settings
  ;; (flyover-checkers '(flymake))
  (flyover-checkers '(flymake))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)

  ;; Text tinting
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icon tinting (foreground and background)
  (flyover-icon-tint 'lighter)
  (flyover-icon-tint-percent 50)
  (flyover-icon-background-tint 'darker)
  (flyover-icon-background-tint-percent 50)

  ;; Icons
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon " ")

  ;; Border styles: none, pill, arrow, slant, slant-inv, flames, pixels
  (flyover-border-style 'pill)
  (flyover-border-match-icon t)

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2)
  (flyover-cursor-debounce-interval 0.3)

  ;; Display mode (controls cursor-based visibility)
  (flyover-display-mode 'always)

  ;; Completion integration
  (flyover-hide-during-completion t)
  :config
  (unless (advice-member-p #'+flyover-create-next-line-overlay
                           'flyover--create-overlay)
    (advice-add 'flyover--create-overlay
                :around #'+flyover-create-next-line-overlay)))

(use-package breadcrumb)

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :hook (prog-mode . +dumb-jump-maybe-enable)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

;;;###autoload
(defun +dumb-jump-xref-activate-unless-eglot ()
  "Return the dumb-jump xref backend unless eglot is managing the buffer."
  (unless (bound-and-true-p eglot--managed-mode)
    (dumb-jump-xref-activate)))

;;;###autoload
(defun +dumb-jump-maybe-enable ()
  "Enable dumb-jump as an xref backend in non-elisp `prog-mode' buffers."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (add-hook 'xref-backend-functions
              #'+dumb-jump-xref-activate-unless-eglot nil t)))
