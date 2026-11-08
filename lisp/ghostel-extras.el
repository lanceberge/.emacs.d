;;; -*- lexical-binding: t -*-
(require 'ghostel)
(require 'agent-lisp)
(require 'modal)
(require 'consult)
(require 'key-chord)

(defvar-local +ghostel-completion-handoff-p nil
  "Non-nil while Bash owns input for completion in semi-char mode.")

(defvar-local +ghostel-command-running nil)

(defvar +ghostel-llm-command "codex")
(defvar +ghostel-llm-buffer-base-name "Codex")
(defvar +ghostel-llm--buffers (make-hash-table :test #'equal)
  "Project roots mapped to tables of slot numbers and live buffers.")

(defvar +ghostel-llm-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode +ghostel-llm-mode
  "Mark this Ghostel buffer as an LLM session."
  :lighter nil
  :keymap +ghostel-llm-mode-map
  (if +ghostel-llm-mode
      (add-hook '+tabspace-other-buffer-pre-hook
                #'+ghostel-copy-mode-normal nil t)
    (remove-hook '+tabspace-other-buffer-pre-hook
                 #'+ghostel-copy-mode-normal t))
  (when (derived-mode-p 'ghostel-mode)
    (+ghostel-override-insert-mode-key-chords)))

(defvar-local +ghostel-llm--first-prompt-seen nil
  "Non-nil after the first LLM prompt is submitted in this buffer.")

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
  "Switch to a numbered LLM buffer for the current project.
With numeric prefix ARG, use slot ARG.  With a non-numeric prefix,
create a buffer in the lowest free slot.  Without a prefix, use slot 1."
  (interactive "P")
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (slot (let ((slot (cond
                            ((null arg) 1)
                            ((numberp arg) arg)
                            (t (+ghostel-llm--next-available-slot root)))))
                 (unless (and (integerp slot) (> slot 0))
                   (user-error "LLM buffer number must be a positive integer"))
                 slot))
         (name-prefix (format "%s <%d>"
                              (+ghostel-llm--buffer-name root) slot))
         (buffer (+ghostel-llm--buffer-for-slot root slot))
         (ghostel-initial-input-mode nil))
    (if buffer
        (let ((switch-to-buffer-obey-display-actions nil))
          (switch-to-buffer buffer nil t))
      (let ((ghostel-buffer-name (format "*ghostel-llm: %s*" name-prefix)))
        (setq buffer (ghostel '(4)))
        (with-current-buffer buffer
          (rename-buffer name-prefix t)
          (+ghostel-llm-mode 1)
          (setq-local +ghostel-llm--first-prompt-seen nil)
          (+ghostel-llm--register-buffer root slot buffer)
          (ghostel-send-string
           (format "GHOSTEL_LLM_BUFFER_NAME=%s %s\n"
                   (shell-quote-argument ghostel--buffer-identity)
                   +ghostel-llm-command))
          (setq-local +ghostel-command-running t))))
    buffer))

;;;###autoload
(defun +ghostel-llm--first-prompt (buffer prompt)
  "Title BUFFER from its first submitted PROMPT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless +ghostel-llm--first-prompt-seen
        (setq +ghostel-llm--first-prompt-seen t)
        (let ((name-prefix (replace-regexp-in-string
                            " <[0-9]+>\\'" "" (buffer-name))))
          (+agent-lisp-title-from-prompt-async
           prompt
           (lambda (title)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (rename-buffer
                  (generate-new-buffer-name
                   (format "%s %s" name-prefix title))))))))))))

;;;###autoload
(defun +ghostel-llm--buffer-for-slot (root slot)
  "Return the LLM buffer in project ROOT and SLOT."
  (when-let* ((slots (gethash root +ghostel-llm--buffers))
              (buffer (gethash slot slots)))
    (when (buffer-live-p buffer)
      buffer)))

;;;###autoload
(defun +ghostel-llm--next-available-slot (root)
  "Return the lowest unused LLM buffer slot in project ROOT."
  (let ((slot 1))
    (while (+ghostel-llm--buffer-for-slot root slot)
      (setq slot (1+ slot)))
    slot))

;;;###autoload
(defun +ghostel-llm--register-buffer (root slot buffer)
  "Register BUFFER in SLOT for project ROOT until the buffer is killed."
  (let ((slots (or (gethash root +ghostel-llm--buffers)
                   (puthash root (make-hash-table :test #'eql)
                            +ghostel-llm--buffers))))
    (puthash slot buffer slots)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq (gethash slot slots) (current-buffer))
                    (remhash slot slots)
                    (when (zerop (hash-table-count slots))
                      (remhash root +ghostel-llm--buffers))))
                nil t))))

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
(defun +ghostel-copy-mode-normal ()
  (interactive)
  (ghostel-copy-mode)
  (+normal-mode 1))

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
             (or (eq ghostel--input-mode 'copy)
                 (eq ghostel--input-mode 'line))
             +ghostel-command-running)
    (ghostel-semi-char-mode)))

;;;###autoload
(defun +ghostel-goto-last-line ()
  "Go to the end of the buffer unless point is on its last line."
  (interactive)
  (unless (= (pos-eol) (point-max))
    (goto-char (point-max))))

;;;###autoload
(defun +ghostel-new ()
  (interactive)
  (ghostel '(4)))

(provide 'ghostel-extras)
