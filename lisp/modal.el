;;; -*- lexical-binding: t -*-
(defvar +leader-map (make-sparse-keymap))
(defvar +leader2-map (make-sparse-keymap))
(defvar +leader3-map (make-sparse-keymap))

(defvar mark-forward-keymap (make-sparse-keymap))
(defvar mark-backward-keymap (make-sparse-keymap))

(defvar +normal-mode-map (make-sparse-keymap))
(defvar +insert-mode-map (make-sparse-keymap))
(defvar +motion-mode-map (make-sparse-keymap))
(defvar +sexp-mode-map (make-keymap))

(defvar-local +modal-desired-state nil
  "Buffer-local override for the desired modal state.
Can be 'normal, 'insert, 'motion, or 'sexp.")

(defvar +modal--switching nil
  "Flag to prevent recursion when switching between modal states.")

(suppress-keymap +normal-mode-map t)
(suppress-keymap +motion-mode-map t)
(suppress-keymap +sexp-mode-map t)

;;; Mode-specific overrides

;;;###autoload
(defun +modal-bind (target keymap-or-hook hook-or-bindings &optional bindings)
  "Override TARGET keys in buffers where HOOK runs.
TARGET is either a modal mode map, modal minor mode command, or prefix map symbol.
BINDINGS are alist entries of the form (KEY . DEF)."
  (let ((mode (+modal--mode-for-map target)))
    (cond
     (mode
      (+modal--command-bind mode target keymap-or-hook hook-or-bindings))
     ((and (boundp target)
           (keymapp (symbol-value target)))
      (+modal--prefix-bind target keymap-or-hook hook-or-bindings))
     ((commandp target)
      (+modal--command-bind target keymap-or-hook hook-or-bindings bindings))
     (t
      (error "Expected modal mode map, modal command, or prefix map symbol: %S"
             target)))))

;;;###autoload
(defun +modal--command-bind (mode keymap hook bindings)
  "Override MODE keys from KEYMAP in buffers where HOOK runs.
BINDINGS are alist entries of the form (KEY . DEF)."
  (unless (and (boundp keymap)
               (keymapp (symbol-value keymap)))
    (error "Expected keymap symbol: %S" keymap))
  (add-hook hook
            (lambda ()
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent
                 map
                 (or (cdr (assq mode minor-mode-overriding-map-alist))
                     (symbol-value keymap)))
                (+modal--define-bindings map bindings)
                (setq-local minor-mode-overriding-map-alist
                            (cons
                             (cons mode map)
                             (assq-delete-all
                              mode
                              minor-mode-overriding-map-alist)))))
            t))

;;;###autoload
(defun +modal--prefix-bind (prefix-map hook bindings)
  "Override PREFIX-MAP keys in modal maps for buffers where HOOK runs.
BINDINGS are alist entries of the form (KEY . DEF)."
  (add-hook hook
            (lambda ()
              (let ((prefix-map-value (symbol-value prefix-map))
                    (map (make-sparse-keymap)))
                (set-keymap-parent map (symbol-value prefix-map))
                (+modal--define-bindings map bindings)
                (dolist (entry minor-mode-map-alist)
                  (let ((mode (car entry))
                        (mode-map (cdr entry))
                        keys)
                    (when (keymapp mode-map)
                      (map-keymap
                       (lambda (key definition)
                         (when (eq definition prefix-map-value)
                           (push key keys)))
                       mode-map)
                      (when keys
                        (let ((mode-map-copy
                               (copy-keymap
                                (or (cdr (assq mode minor-mode-overriding-map-alist))
                                    mode-map))))
                          (dolist (key keys)
                            (define-key mode-map-copy (vector key) map))
                          (setq-local minor-mode-overriding-map-alist
                                      (cons
                                       (cons mode mode-map-copy)
                                       (assq-delete-all
                                        mode
                                        minor-mode-overriding-map-alist))))))))))
            t))

;;;###autoload
(defun +modal--mode-for-map (keymap)
  "Return the modal command derived from KEYMAP, or nil."
  (when (and (symbolp keymap)
             (boundp keymap)
             (keymapp (symbol-value keymap)))
    (let* ((name (symbol-name keymap))
           (mode-name (and (string-suffix-p "-map" name)
                           (substring name 0 (- (length "-map")))))
           (mode (and mode-name (intern-soft mode-name))))
      (and (commandp mode) mode))))

;;;###autoload
(defun +modal--define-bindings (map bindings)
  "Define BINDINGS in MAP.
BINDINGS are alist entries of the form (KEY . DEF)."
  (dolist (binding bindings)
    (define-key map
                (if (stringp (car binding))
                    (kbd (car binding))
                  (car binding))
                (cdr binding))))

(defmacro +modal-create-mode-switching-function (function &rest args)
  "Create a command wrapping FUNCTION and switching modes afterward.
ARGS must provide an `:ending-mode-function' atom.  The created
command is named `+modal-FUNCTION-MODE', unless ARGS provides a
`:name' atom."
  (declare (indent defun))
  (let* ((name (plist-get args :name))
         (ending-mode-function (plist-get args :ending-mode-function))
         (function-name (symbol-name function))
         (function-name (if (string-prefix-p "+" function-name)
                            (substring function-name 1)
                          function-name))
         (mode-name (and ending-mode-function
                         (symbol-name ending-mode-function)))
         (mode-name (and mode-name
                         (if (string-prefix-p "+" mode-name)
                             (substring mode-name 1)
                           mode-name)))
         (mode-name (and mode-name
                         (if (string-match-p "-mode\\'" mode-name)
                             (substring mode-name 0 (- (length "-mode")))
                           mode-name)))
         (fn (or name
                 (intern (format "+modal-%s-%s" function-name mode-name))))
         (valid-args t))
    (let ((remaining args))
      (while remaining
        (if (and (memq (car remaining) '(:name :ending-mode-function))
                 (cdr remaining))
            (setq remaining (cddr remaining))
          (setq valid-args nil
                remaining nil))))
    (when (or (not ending-mode-function)
              (not (symbolp ending-mode-function))
              (and name (not (symbolp name)))
              (not valid-args))
      (error "Expected :ending-mode-function atom and optional :name atom"))
    `(progn
       ;;;###autoload
       (defun ,fn (&rest args)
         ,(format "Call `%s' interactively, then switch to `%s'."
                  function ending-mode-function)
         (interactive)
         (if args
             (apply #',function args)
           (call-interactively #',function))
         (,ending-mode-function 1)))))

(defmacro +modal-create-insert-function (function &rest args)
  "Create an insert-mode command wrapping FUNCTION.
The created command is named `+modal-FUNCTION-insert', unless
ARGS provides a `:name' atom."
  (declare (indent defun))
  `(+modal-create-mode-switching-function ,function
     :ending-mode-function +insert-mode
     ,@args))
;;; Mode-line indicator

(defun +modal--mode-line-indicator ()
  "Return a string indicating the current modal state."
  (cond
   (+sexp-mode "SEXP")
   (+insert-mode "INSERT")
   (+motion-mode "MOTION")
   (+normal-mode "NORMAL")
   (t "")))

;;; Minor Modes

(define-minor-mode +normal-mode
  "Normal mode for modal editing."
  :lighter " [N]"
  :keymap +normal-mode-map
  (when (and +normal-mode (not +modal--switching))
    (let ((+modal--switching t))
      (+insert-mode -1)
      (+motion-mode -1)
      (+sexp-mode -1))
    (setq-local cursor-type 'box)))

(define-minor-mode +insert-mode
  "Insert mode for modal editing."
  :lighter " [I]"
  :keymap +insert-mode-map
  (when (and +insert-mode (not +modal--switching))
    (let ((+modal--switching t))
      (+normal-mode -1)
      (+motion-mode -1)
      (+sexp-mode -1))
    (setq-local cursor-type '(bar . 2))))

(define-minor-mode +motion-mode
  "Motion mode for read-only/special buffers."
  :lighter " [M]"
  :keymap +motion-mode-map
  (when (and +motion-mode (not +modal--switching))
    (let ((+modal--switching t))
      (+normal-mode -1)
      (+insert-mode -1)
      (+sexp-mode -1))
    (setq-local cursor-type 'box)))

(define-minor-mode +sexp-mode
  "Sexp mode for structural editing."
  :lighter " [P]"
  :keymap +sexp-mode-map
  (when (and +sexp-mode (not +modal--switching))
    (let ((+modal--switching t))
      (+normal-mode -1)
      (+insert-mode -1)
      (+motion-mode -1))
    (setq-local cursor-type 'box)))

;;;###autoload
(defun +modal-insert-advice (&rest _)
  "Enter insert mode after an advised command."
  (+insert-mode 1))

;;;###autoload
(defun +modal-normal-advice (&rest _)
  "Enter normal mode after an advised command."
  (+normal-mode 1))

;; Suppress "enabled/disabled in current buffer" messages
(dolist (mode '(+normal-mode +insert-mode +motion-mode +sexp-mode))
  (advice-add mode :around
              (lambda (orig &rest args)
                (let ((inhibit-message t))
                  (apply orig args)))))

;;; Global Activation
(defun +modal--desired-state ()
  "Determine what modal state the current buffer should be in."
  (cond
   (+modal-desired-state +modal-desired-state)
   ((or (minibufferp) (derived-mode-p 'eshell-mode 'eat-mode 'ghostel-mode 'agent-shell-mode 'opencode-session-mode))
    'insert)
   ((derived-mode-p 'special-mode 'dired-mode 'magit-mode 'org-agenda-mode
                    'help-mode 'Info-mode 'compilation-mode
                    'diff-mode 'package-menu-mode
                    'Custom-mode 'messages-buffer-mode) 'motion)
   (t 'normal)))

(defun +modal--turn-on ()
  "Activate the appropriate modal state for the current buffer."
  (pcase (+modal--desired-state)
    ('none nil)
    ('insert (+insert-mode 1))
    ('motion (+motion-mode 1))
    ('sexp (+sexp-mode 1))
    (_ (+normal-mode 1))))

(add-hook 'after-change-major-mode-hook #'+modal--turn-on)
(add-hook 'minibuffer-setup-hook (lambda () (+insert-mode 1)))

;;; Digit keys - universal argument in normal + sexp modes

(dolist (i (number-sequence 1 9))
  (define-key +sexp-mode-map
              (number-to-string i)
              `(lambda ()
                 (interactive)
                 (setq prefix-arg ,i)
                 (universal-argument--mode)))

  (define-key +normal-mode-map
              (number-to-string i)
              `(lambda ()
                 (interactive)
                 (setq prefix-arg ,i)
                 (universal-argument--mode))))

(define-key +normal-mode-map "0" #'universal-argument)

;;;###autoload
(defun +insert-char-overwrite-region (c)
  (interactive)
  (when (region-active-p)
    (call-interactively #'kill-region))
  (insert-char c))

;;; Insert entries
(dolist (function '(+kill-line
                    delete-char
                    backward-delete-char
                    forward-char
                    backward-char
                    forward-word
                    backward-word
                    end-of-line
                    beginning-of-visual-line
                    back-to-indentation
                    kill-word
                    kill-region
                    next-line
                    previous-line
                    backward-kill-word
                    zap-up-to-char
                    kill-sexp
                    +start-of-region-or-backward-char
                    +end-of-region-or-forward-char
                    end-of-buffer))
  (eval `(+modal-create-insert-function ,function)))

;;;###autoload
(defun +kill-line (arg)
  "Kill region, or kill visual line if no region is active."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-visual-line arg)))

;;; Insert Exits
;;;###autoload
(defun +save-buffer-normal (arg)
  (interactive "p")
  (save-buffer arg)
  (+normal-mode))

;;;###autoload
(defun +start-of-region-or-backward-char (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (let ((dest (min (point) (mark))))
        (goto-char (- dest (or arg 0)))
        (deactivate-mark t))
    (backward-char (or arg 1))))

;;;###autoload
(defun +end-of-region-or-forward-char (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (let ((dest (max (point) (mark))))
        (goto-char (+ dest (or arg 0)))
        (deactivate-mark t))
    (forward-char (or arg 1))))

;;; Modal Utils
;;;###autoload
(defun +modal-mark-end-of-line (arg)
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))
  (end-of-line arg))

;;;###autoload
(defun +modal-mark-beginning-of-line (arg)
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))
  (beginning-of-line arg))

;;;###autoload
(defun +left-expand (arg)
  "Set mark if no region, then move backward one char."
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))
  (backward-char arg))

;;;###autoload
(defun +right-expand (arg)
  "Set mark if no region, then move forward one char."
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))
  (forward-char arg))

;;;###autoload
(defun +mark-whole-lines (_arg)
  "Select the current line."
  (interactive "p")
  (if (region-active-p)
      (progn
        (call-interactively #'next-line)
        (beginning-of-visual-line))
    (push (copy-marker (point)) mark-ring)
    (beginning-of-visual-line)
    (set-mark (point))
    (call-interactively #'next-line)
    (beginning-of-visual-line)
    (activate-mark)))

;;;###autoload
(defun +cancel-selection ()
  "Deactivate the mark."
  (interactive)
  (deactivate-mark))

;;;###autoload
(defun +keyboard-quit-normal ()
  (interactive)
  (+normal-mode 1)
  (+keyboard-quit))

;;;###autoload
(defun +save-and-exit ()
  (interactive)
  (save-buffer)
  (+normal-mode 1))

;;;###autoload
(defun +sexp-mode-quit ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (+normal-mode 1)))

;;;###autoload
(defun +open-line (arg)
  (interactive "P")
  (cond
   (arg
    (if (eq (point) (save-excursion (end-of-line) (point)))
        (open-line arg)
      (save-excursion (beginning-of-line) (open-line arg))))
   ;; if at the end of a non-empty line, insert above
   ((and (not (bolp)) (eolp))
    (open-line (or arg 1))
    (forward-line 1)
    (indent-according-to-mode)
    (+insert-mode 1))
   ;; insert below
   (t
    (beginning-of-visual-line)
    (open-line (or arg 1))
    (indent-according-to-mode)
    (+insert-mode 1))))

;;;###autoload
(defun +backward-kill-sexp ()
  (interactive)
  (forward-char 1)
  (backward-kill-sexp))

;;;###autoload
(defun +kill-line-or-region (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (save-excursion
      (beginning-of-line)
      (kill-visual-line arg))))

;;;###autoload
(defun +replace-char (char)
  (interactive "cChar:")
  (delete-char 1)
  (insert-char char)
  (backward-char 1))

;;;###autoload
(defun +kill-line-dwim (arg col)
  "Kill `ARG' whole lines or run `kill-line' if no arg is provided"
  (interactive (list
                current-prefix-arg
                (current-column)))
  (if arg
      (progn
        (beginning-of-line)
        (kill-line arg)
        (move-to-column col))
    (kill-line)))

;; Nice mark utils taken from meow
;;;###autoload
(defun +unpop-to-mark ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

;;;###autoload
(defun +pop-to-mark ()
  (interactive)
  (unless (member last-command '(+unpop-to-mark))
    (setq mark-ring (append mark-ring (list (point-marker)))))
  (pop-to-mark-command))

;;;###autoload
(define-minor-mode +modal-org-mode
  "Modal editing integration for Org buffers."
  :lighter nil
  (if +modal-org-mode
      (progn
        (add-hook 'org-insert-heading-hook #'+insert-mode nil t))
    (remove-hook 'org-insert-heading-hook #'+insert-mode t)))

(provide 'modal)
