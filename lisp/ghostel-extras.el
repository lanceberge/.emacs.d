;;; -*- lexical-binding: t -*-
(require 'ghostel)
(require 'modal)
(require 'consult)
(require 'key-chord)

(defvar-local +ghostel-completion-handoff-p nil
  "Non-nil while Bash owns input for completion in semi-char mode.")

(defvar +ghostel-llm-command "codex")
(defvar +ghostel-llm-buffer-base-name "Codex")

;;;###autoload
(defun +ghostel-semi-char-tab ()
  "Hand line-mode input to Bash and request tab completion."
  (interactive)
  (setq +ghostel-completion-handoff-p t)
  (ghostel-semi-char-mode)
  (+insert-mode 1)
  (ghostel-send-key "tab"))

;;;###autoload
(defun +ghostel-llm (&optional arg)
  "Start Ghostel and run codexp.
ARG is passed through to `ghostel'."
  (interactive "P")
  (let ((buffer (ghostel-project arg)))
    (with-current-buffer buffer
      (let ((name (+ghostel-llm--buffer-name)))
        (unless (equal name (buffer-name))
          (rename-buffer name)
          (ghostel-send-string (format "%s\n" +ghostel-llm-command)))))
    (ghostel-semi-char-mode)
    (+insert-mode 1)
    buffer))

;;;###autoload
(defun +ghostel-llm--buffer-name ()
  (let ((base (format "%s: %s" +ghostel-llm-buffer-base-name (+ghostel-llm--project-name))))
    (if (+ghostel-llm--buffer-name-available-p base)
        base
      (+ghostel-llm--deduped-buffer-name base 1))))

;;;###autoload
(defun +ghostel-llm--project-name ()
  "Return the current project's display name."
  (if-let* ((project (project-current nil)))
      (project-name project)
    (file-name-nondirectory
     (directory-file-name default-directory))))

;;;###autoload
(defun +ghostel-llm--deduped-buffer-name (base n)
  "Return the first available buffer name derived from BASE and N."
  (let ((name (format "%s<%d>" base n)))
    (if (+ghostel-llm--buffer-name-available-p name)
        name
      (+ghostel-llm--deduped-buffer-name base (1+ n)))))

;;;###autoload
(defun +ghostel-llm--buffer-name-available-p (name)
  "Return non-nil when NAME is unused or names the current buffer."
  (let ((buffer (get-buffer name)))
    (or (not buffer)
        (eq buffer (current-buffer)))))

;;;###autoload
(defun +ghostel-semi-char-return ()
  "Submit Bash-completed input through Ghostel line mode."
  (interactive)
  (if (not +ghostel-completion-handoff-p)
      (ghostel-send-key "return")
    (ghostel-line-mode)
    (setq +ghostel-completion-handoff-p nil)
    (ghostel-line-mode-send)))

;;;###autoload
(defun +ghostel-use-modal-cursor (&rest _)
  "Set the Ghostel cursor shape from the active modal state."
  (setq-local cursor-type (if +insert-mode '(bar . 2) 'box)))

;;;###autoload
(defun +ghostel-consult-history ()
  "Select a Ghostel line-mode history entry with `consult-history'."
  (interactive)
  (unless (eq ghostel--input-mode 'line)
    (ghostel-line-mode))
  (unless (eq ghostel--input-mode 'line)
    ;; TODO send C-r
    (user-error "Ghostel line mode is not active"))
  (consult-history ghostel--line-mode-history
                   'ghostel--line-mode-history-index
                   #'ghostel-beginning-of-input-or-line))

;;;###autoload
(defun +ghostel-line-mode-normal ()
  (interactive)
  (ghostel-line-mode 1)
  (+normal-mode 1))

;;;###autoload
;;;###autoload
(defun +ghostel-override-insert-mode-key-chords ()
  "Override insert-mode key chords in the current Ghostel buffer."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map +insert-mode-map)
    (key-chord-define map "jk" #'+ghostel-line-mode-normal)
    (setq-local minor-mode-overriding-map-alist
                (cons (cons '+insert-mode map)
                      (assq-delete-all '+insert-mode
                                       minor-mode-overriding-map-alist)))))

;;;###autoload
(defun +ghostel-reset-point ()
  (interactive)
  (ghostel-semi-char-mode)
  (ghostel-line-mode)
  (+insert-mode 1))

(provide 'ghostel-extras)
