;;; -*- lexical-binding: t -*-
(require 'project)
(require 'eat)
(require 'modal)
(require 'ghostel-extras)

;;;###autoload
(defun +eat-llm (&optional arg)
  "Start the LLM command in a project-specific Eat buffer.
With numeric prefix ARG, use the correspondingly numbered buffer.
With a non-numeric prefix ARG, create the next available buffer."
  (interactive "P")
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (name (+ghostel-llm--buffer-name root arg))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode)))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buffer name "/usr/bin/env" nil
                  (list (funcall eat-default-shell-function)
                        "-l" "-i" "-c" +ghostel-llm-command)))
      (setq-local +eat--command-running t))
    buffer))

;;;###autoload
(defun +eat-semi-char-tab ()
  "Switch Eat to semi-char mode and send a tab input event.
I use this to tab-complete in eat buffers with bash completions instead of `completion-at-point'."
  (interactive)
  (eat-semi-char-mode)
  (+insert-mode 1)
  (eat-self-input 1 ?\t))

(defvar-local +eat--command-running nil
  "Non-nil while the terminal's shell is running a command.")

;;;###autoload
(defun +eat-insert-mode-reevaluate ()
  "Leave `eat-line-mode' for semi-char mode if a shell command is running.
Intended for `+insert-mode-hook', so that entering insert mode in
line mode drops into semi-char mode when the shell is busy."
  (when (and +insert-mode
             (bound-and-true-p eat--line-mode)
             +eat--command-running)
    (eat-semi-char-mode)))

;;;###autoload
(defun +eat-eshell-insert-mode-reevaluate ()
  "Enter Eat Eshell char mode when inserting during a running command.
Intended for `+insert-mode-hook' in Eshell buffers."
  (when (and +insert-mode
             (bound-and-true-p eat--eshell-local-mode)
             (bound-and-true-p eat--eshell-process-running-mode))
    (eat-eshell-char-mode)))

;;;###autoload
(defun +eat--command-started (&rest _)
  "Record that the shell started running a command."
  (setq-local +eat--command-running t))

;;;###autoload
(defun +eat--command-finished (&rest _)
  "Record that the shell finished running a command."
  (setq-local +eat--command-running nil))

;;;###autoload
(defun +eat-eshell-use-modal-cursor ()
  (when (bound-and-true-p eat-terminal)
    (eat-term-set-parameter eat-terminal 'set-cursor-function #'ignore)))

;;;###autoload
(defun +eat--record-shell-command-in-line-history (encoded-command)
  "Record Eat shell integration ENCODED-COMMAND in line input history."
  (when-let* ((command (ignore-errors
                         (decode-coding-string
                          (base64-decode-string encoded-command)
                          locale-coding-system))))
    (+eat--add-to-line-input-history command)))

;;;###autoload
(defun +eat--add-to-line-input-history (command)
  "Add COMMAND to Eat's line input history for the current buffer."
  (unless (or (string-empty-p command)
              (and (ring-p eat--line-input-ring)
                   (not (ring-empty-p eat--line-input-ring))
                   (equal command (ring-ref eat--line-input-ring 0))))
    (unless eat--line-input-ring
      (setq eat--line-input-ring
            (make-ring eat-line-input-ring-size)))
    (ring-insert eat--line-input-ring command)
    (eat--line-reset-input-ring-vars)))

;;;###autoload
(defun +eat-line-mode-normal ()
  (interactive)
  (eat-line-mode)
  (+normal-mode))

;;;###autoload
(defun +eat-eshell-emacs-mode-normal ()
  "Switch to Eat Eshell Emacs mode and enter modal normal mode."
  (interactive)
  (eat-eshell-emacs-mode)
  (+normal-mode))

(provide 'eat-extras)
