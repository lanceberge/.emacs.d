;;; -*- lexical-binding: t -*-
;; Custom modal editing system with 4 mutually exclusive minor modes.
;; Keymaps (+normal-mode-map, +insert-mode-map, +motion-mode-map, +sexp-mode-map)
;; are declared in init.el so bind-key calls work before this file loads.

(defvar-local +modal-desired-state nil
  "Buffer-local override for the desired modal state.
Can be 'normal, 'insert, 'motion, or 'sexp.")

(defvar +modal--switching nil
  "Flag to prevent recursion when switching between modal states.")

(suppress-keymap +normal-mode-map t)
(suppress-keymap +motion-mode-map t)
(suppress-keymap +sexp-mode-map t)

;;; Mode-specific overrides

(defmacro +modal-bind (modal-mode hook &rest bindings)
  "Override MODAL-MODE keys in buffers where HOOK runs.
BINDINGS are alternating KEY DEF pairs.  Creates a buffer-local
keymap that inherits from MODAL-MODE's base map with the given
overrides applied via `minor-mode-overriding-map-alist'."
  (let ((fn (intern (format "+modal--override-%s-for-%s"
                            (symbol-name modal-mode)
                            (symbol-name hook))))
        (map-var (intern (format "%s-map" (symbol-name modal-mode))))
        (binding-forms '()))
    (while bindings
      (push `(define-key map ,(pop bindings) ,(pop bindings)) binding-forms))
    (setq binding-forms (nreverse binding-forms))
    `(progn
       (defun ,fn ()
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map ,map-var)
           ,@binding-forms
           (setq-local minor-mode-overriding-map-alist
                       (cons (cons ',modal-mode map)
                             minor-mode-overriding-map-alist))))
       (add-hook ',hook #',fn))))
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
   ((minibufferp) 'insert)
   ((derived-mode-p 'special-mode 'dired-mode 'magit-mode
                    'help-mode 'Info-mode 'compilation-mode
                    'diff-mode 'package-menu-mode
                    'Custom-mode 'messages-buffer-mode) 'motion)
   (t 'normal)))

(defun +modal--turn-on ()
  "Activate the appropriate modal state for the current buffer."
  (pcase (+modal--desired-state)
    ('insert (+insert-mode 1))
    ('motion (+motion-mode 1))
    ('sexp (+sexp-mode 1))
    (_ (+normal-mode 1))))

(add-hook 'after-change-major-mode-hook #'+modal--turn-on)
(add-hook 'minibuffer-setup-hook (lambda () (+insert-mode 1)))

;;; Digit keys - universal argument in normal + sexp modes

(dotimes (i 10)
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

;;; Escape with jk or kj
;;;###autoload
(defun +create-escape (keys)
  "Create and bind a two-key escape sequence KEYS in insert mode.
KEYS is a 2-char string like \"jk\". Pressing the first char waits
briefly for the second; if it arrives, exit to normal mode.
Otherwise insert the first char and handle the second normally."
  (let ((first (aref keys 0))
        (second (aref keys 1)))
    (define-key +insert-mode-map (string first)
                (let ((f first) (s second))
                  (lambda ()
                    (interactive)
                    (+escape--handle f s)))))
  keys)

(defun +escape--handle (first second)
  (let* ((cooldown 0.5)
         (char (read-char nil nil cooldown)))
    (if (null char)
        (+insert-char-overwrite-region first)
      (if (= char second)
          (progn
            (when (bound-and-true-p corfu--frame)
              (corfu-quit))
            (+normal-mode 1))
        (+insert-char-overwrite-region first)
        (let ((command (key-binding (vector char))))
          (cond
           ((null command))
           ((not (commandp command)))
           ((and (eq char ?\") (eq (char-after (point)) ?\"))
            (forward-char))
           ((memq command '(self-insert-command org-self-insert-command))
            (+insert-char-overwrite-region char))
           (t
            (call-interactively command))))))))

;;;###autoload
(defun +insert-char-overwrite-region (c)
  (interactive)
  (when (region-active-p)
    (call-interactively #'kill-region))
  (insert-char c))

(+create-escape "jk")
(+create-escape "kj")

;;; Insert entries
;;;###autoload
(defun +kill-line-insert ()
  "Kill region (or kill-visual-line if no region) then enter insert mode."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-visual-line))
  (+insert-mode 1))

;;;###autoload
(defun +delete-char-insert (arg)
  (interactive "p")
  (delete-char arg)
  (+insert-mode))

;;;###autoload
(defun +forward-char-insert (arg)
  (interactive "p")
  (forward-char arg)
  (+insert-mode 1))

;;;###autoload
(defun +backward-char-insert (arg)
  (interactive "p")
  (backward-char arg)
  (+insert-mode 1))

;;;###autoload
(defun +forward-word-insert (arg)
  (interactive "p")
  (forward-word arg)
  (+insert-mode 1))

;;;###autoload
(defun +backward-word-insert (arg)
  (interactive "p")
  (backward-word arg)
  (+insert-mode 1))

;;;###autoload
(defun +end-of-line-insert ()
  (interactive)
  (end-of-line)
  (+insert-mode 1))

;;;###autoload
(defun +beginning-of-line-insert ()
  (interactive)
  (beginning-of-visual-line)
  (+insert-mode 1))

;;;###autoload
(defun +back-to-indentation-insert ()
  (interactive)
  (back-to-indentation)
  (+insert-mode 1))

;;;###autoload
(defun +mark-forward-insert ()
  (interactive)
  (+mark-forward-word)
  (+insert-mode 1))

;;;###autoload
(defun +mark-backward-insert ()
  (interactive)
  (+mark-backward-word)
  (+insert-mode 1))

;;;###autoload
(defun +delete-word-insert (arg)
  (interactive "p")
  (kill-word arg)
  (+insert-mode 1))

;;;###autoload
(defun +kill-region-insert ()
  (interactive)
  (call-interactively #'kill-region)
  (+insert-mode 1))

;;;###autoload
(defun +mark-forward-char-insert (arg)
  (interactive "p")
  (+mark-forward-char arg)
  (+insert-mode 1))

;;;###autoload
(defun +mark-backward-char-insert (arg)
  (interactive "p")
  (+mark-backward-char arg)
  (+insert-mode 1))

;;;###autoload
(defun +next-line-insert (arg)
  (interactive "p")
  (next-line arg)
  (+insert-mode 1))

;;;###autoload
(defun +previous-line-insert (arg)
  (interactive "p")
  (previous-line arg)
  (+insert-mode 1))

;;; Modal Utils

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
(defun +mark-whole-line (&optional arg)
  "Select the current line."
  (interactive "P")
  (let ((arg (or arg 1)))
    (if (region-active-p)
        (next-line arg)
      (beginning-of-line)
      (set-mark (point))
      (next-line arg)
      (activate-mark))))

;;;###autoload
(defun +open-below (arg)
  "Open a newline below and switch to insert mode."
  (interactive "p")
  (goto-char (line-end-position))
  (newline arg)
  (indent-according-to-mode)
  (+insert-mode 1))

;;;###autoload
(defun +open-above ()
  "Open a newline above and switch to insert mode."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode)
  (+insert-mode 1))

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
  (cond (arg
         (if (eq (point) (save-excursion (end-of-line) (point)))
             (open-line arg)
           (save-excursion (beginning-of-line) (open-line arg))))
        ((eq (point) (save-excursion (end-of-line) (point)))
         (open-line (or arg 1))
         (next-line)
         (indent-according-to-mode)
         (+insert-mode 1))
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
