;;; -*- lexical-binding: t -*-
(require 'ghostel-extras)
(require 'consult-extras)
(require 'project)
(require 'subr-x)

(defcustom +ghostel-llm-command "codex"
  "Command used to start an LLM in Ghostel."
  :type 'string
  :group 'ghostel)
(defcustom +ghostel-llm-buffer-base-name "Codex"
  "Base name used for LLM session buffers."
  :type 'string
  :group 'ghostel)
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

(defvar-local +ghostel-llm--name-prefix nil
  "Project and slot portion of this LLM buffer's name.")
(defvar-local +ghostel-llm--description nil
  "Description reported by the LLM terminal title.")
(defvar-local +ghostel-llm--status nil
  "Current status of this LLM session.")

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
          (with-current-buffer buffer
            (+ghostel-llm--update-buffer-name))
          (switch-to-buffer buffer nil t))
      (let ((ghostel-buffer-name (format "*ghostel-llm: %s <%d>*" root slot)))
        (setq buffer (ghostel '(4)))
        (with-current-buffer buffer
          (setq-local +ghostel-llm--name-prefix name-prefix)
          (setq-local +ghostel-llm--status "Ready")
          (+ghostel-llm--update-buffer-name)
          (+ghostel-llm-mode 1)
          (+ghostel-llm--register-buffer root slot buffer)
          (ghostel-send-string
           (format "GHOSTEL_LLM_BUFFER_NAME=%s %s\n"
                   (shell-quote-argument (alist-get 'name ghostel-identity))
                   +ghostel-llm-command))
          (setq-local +ghostel-command-running t))))
    buffer))

;;;###autoload
(defun +ghostel-llm-consult-buffer ()
  "Select a Ghostel LLM buffer with Consult."
  (interactive)
  (+consult-buffer-with-initial
   (format "%s: " +ghostel-llm-buffer-base-name)))

;;;###autoload
(defun +ghostel-llm--set-status (buffer status)
  "Set BUFFER's LLM STATUS and update its name."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local +ghostel-llm--status status)
      (+ghostel-llm--update-buffer-name))))

;;;###autoload
(defun +ghostel-llm--update-buffer-name ()
  "Name the current LLM buffer from its project, description, and status."
  (when +ghostel-llm--name-prefix
    (unless (eq ghostel-buffer-name-function #'+ghostel-llm--name-from-title)
      (setq-local ghostel-buffer-name-function #'+ghostel-llm--name-from-title)
      (setq-local ghostel--managed-buffer-name (buffer-name)))
    (ghostel--rename-managed
     (+ghostel-llm--name-from-title ghostel-title))))

;;;###autoload
(defun +ghostel-llm--name-from-title (title)
  "Update the LLM description from TITLE and return its buffer name."
  (when title
    (let* ((description (string-trim
                         (replace-regexp-in-string "\\`[⠀-⣿[:space:]]+" "" title)))
           (description (string-remove-suffix
                         (concat " | " (+ghostel-llm--project-name))
                         description)))
      (unless (or (string-empty-p description)
                  (equal description (+ghostel-llm--project-name)))
        (setq-local +ghostel-llm--description description))))
  (when +ghostel-llm--name-prefix
    (string-join (delq nil (list +ghostel-llm--name-prefix
                                 +ghostel-llm--description
                                 (and +ghostel-llm--status
                                      (format "[%s]" +ghostel-llm--status))))
                 " ")))

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

(provide 'ghostel-llm)
