;;; -*- lexical-binding: t -*-
(require 'ghostel)
(require 'modal)
(require 'consult)
(require 'key-chord)

(defvar-local +ghostel-completion-handoff-p nil
  "Non-nil while Bash owns input for completion in semi-char mode.")

(defvar-local +ghostel-command-running nil)

(defvar +ghostel-llm-command "codex")
(defvar +ghostel-llm-buffer-base-name "Codex")

;;;###autoload
(defun +ghostel-auto-semi-char-mode (buffer)
  "Enter semi-char mode in BUFFER when a shell command starts."
  (run-at-time 0 nil #'+ghostel-cmd-start buffer))

;;;###autoload
(defun +ghostel-auto-line-mode (buffer _status)
  "Enter line mode in BUFFER when a shell command finishes."
  (run-at-time 0 nil #'+ghostel-cmd-end buffer))

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
  "Start the LLM command in a project-specific Ghostel buffer.
With numeric prefix ARG, use the correspondingly numbered buffer.
With a non-numeric prefix ARG, create the next available buffer."
  (interactive "P")
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (name (+ghostel-llm--buffer-name root arg))
         (buffer (get-buffer name))
         (ghostel-initial-input-mode nil))
    (if buffer
        (let ((switch-to-buffer-obey-display-actions nil))
          (switch-to-buffer buffer nil t))
      (let ((ghostel-buffer-name (format "*ghostel-llm: %s*" name)))
        (setq buffer (ghostel '(4)))
        (with-current-buffer buffer
          (rename-buffer name)
          (ghostel-send-string (format "%s\n" +ghostel-llm-command))
          (setq-local +ghostel-command-running t))))
    buffer))

;;;###autoload
(defun +ghostel-llm--buffer-name (root &optional arg)
  "Return the LLM buffer name for project ROOT and prefix ARG."
  (let* ((default-directory root)
         (base (format "%s: %s"
                       +ghostel-llm-buffer-base-name
                       (+ghostel-llm--project-name)))
         (base (if-let* ((remote (file-remote-p root)))
                   (format "%s@%s" base (string-trim remote "/" ":"))
                 base)))
    (cond
     ((numberp arg) (format "%s<%d>" base arg))
     (arg (+ghostel-llm--next-available-buffer-name base 0))
     (t base))))

;;;###autoload
(defun +ghostel-llm--next-available-buffer-name (base n)
  "Return the first available buffer name derived from BASE and N."
  (let ((name (if (zerop n) base (format "%s<%d>" base n))))
    (if (get-buffer name)
        (+ghostel-llm--next-available-buffer-name base (1+ n))
      name)))

;;;###autoload
(defun +ghostel-llm--project-name ()
  "Return the current project's display name."
  (if-let* ((project (project-current nil)))
      (project-name project)
    (file-name-nondirectory
     (directory-file-name default-directory))))

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
(defun +ghostel-tramp-initial-input-mode ()
  "Force semi-char as the initial input mode on TRAMP hosts.
Meant for `ghostel-mode-hook'; runs before
`ghostel--apply-initial-input-mode' so the buffer-local override
takes effect for the freshly created terminal."
  (when (file-remote-p default-directory)
    (setq-local ghostel-initial-input-mode 'semi-char)))

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

;;;###autoload
(defun +ghostel-cmd-start (buffer)
  "Enter semi-char mode in live Ghostel BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local +ghostel-command-running t)
      (ghostel-semi-char-mode))))

;;;###autoload
(defun +ghostel-cmd-end (buffer)
  "Enter line mode in live Ghostel BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local +ghostel-command-running nil)
      (ghostel-line-mode))))

;;;###autoload
(defun +ghostel-insert-mode-reevaluate ()
  "Leave `eat-line-mode' for semi-char mode if a shell command is running.
Intended for `+insert-mode-hook', so that entering insert mode in
line mode drops into semi-char mode when the shell is busy."
  (when (and +insert-mode
             (eq ghostel--input-mode 'line)
             +ghostel-command-running)
    (ghostel-semi-char-mode)))

(provide 'ghostel-extras)
