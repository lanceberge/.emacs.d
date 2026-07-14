;;; -*- lexical-binding: t -*-
(use-package gptel
  :custom
  (gptel-model 'claude-sonnet-4-6)
  (gptel-default-mode 'org-mode)
  (gptel-cache '(system message tool))
  :config
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

(use-package gptel-extras
  :ensure (:type file :main "~/.emacs.d/lisp/gptel-extras.el" :files ("gptel-extras.el"))
  :demand t
  :bind
  (:map gptel-mode-map
        ("C-c C-l". #'+gptel-project-clear-buffer)
        ([remap org-return] . #'+gptel-send)
        (["C-j"] . #'newline)
        ([remap gptel-send] . #'+gptel-send)
        ("S-<return>" . #'newline))
  (:map +llm-map
        ("g" . #'+gptel-project))
  :config
  (add-hook 'gptel-rewrite-directives-hook #'+gptel-rewrite-directive-from-agents-md))

(use-package agent-lisp
  :ensure (:type file :main "~/.emacs.d/lisp/agent-lisp.el" :files ("agent-lisp.el"))
  :commands
  (+agent-lisp-trace +agent-lisp-apropos +agent-lisp-eval-buffer +agent-lisp-function-source))

(use-package eca
  :custom
  (eca-chat-use-side-window nil)
  (eca-custom-command (list eca-server-install-path "server"))
  :bind
  (:map +llm-map
        ("e" . #'eca)))

(use-package opencode
  :ensure (:host codeberg :repo "sczi/opencode.el")
  :init
  (autoload 'opencode-new-session "opencode" nil t)
  :bind
  (:map opencode-session-control-mode-map
        ;; unbind the default which deletes a session and is too easy to hit
        ("n" . #'next-line)
        ("p" . #'previous-line)
        ("+" . #'+opencode-new-session)
        ("x" . nil))
  (:map +leader-map
        ("nc" . #'opencode-new-session))
  (:map +llm-map
        ("fc" . #'opencode)
        ("c" . #'+opencode-select-open-session))
  (:map opencode-session-mode-map
        ("C-c n" . #'+opencode-new-session))
  :config
  (add-hook 'opencode-session-mode-hook
            (lambda ()
              (add-hook 'whisper-after-insert-hook 'comint-send-input nil t)))
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-name _action)
       (with-current-buffer buffer-name
         (derived-mode-p 'opencode-session-mode
                         'opencode-session-control-mode)))
     (display-buffer-full-frame))))

;;;###autoload
(defun +opencode-select-open-session ()
  "Select an open OpenCode session for the current project, or create one."
  (interactive)
  (require 'opencode)
  (let* ((project (or (project-current)
                      (user-error "Not in a project")))
         (root (project-root project))
         (buffers
          (seq-filter
           (lambda (buffer)
             (and (opencode--session-buffer-p buffer)
                  (with-current-buffer buffer
                    (when-let ((session-project (project-current)))
                      (file-equal-p root (project-root session-project))))))
           (buffer-list))))
    (pcase buffers
      ('nil (let ((default-directory root))
              (opencode-new-session)))
      (`(,buffer) (switch-to-buffer buffer))
      (_ (switch-to-buffer
          (completing-read "Switch to: "
                           (mapcar #'buffer-name buffers) nil t))))))

;;;###autoload
(defun +opencode-new-session ()
  (interactive)
  (require 'opencode)
  (kill-current-buffer)
  (opencode-new-session))

(use-package whisper
  :unless IS-WORK
  :ensure (:host github :repo "natrys/whisper.el")
  :custom
  (whisper-install-whispercpp 'manual)
  (whisper-model "base.en")
  (whisper-language "en")
  :bind
  (:map +leader-map
        ("rn" . #'whisper-run)))
