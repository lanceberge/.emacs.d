;;; -*- lexical-binding: t -*-
;; Custom modal editing system with 4 mutually exclusive minor modes.
;; Keymaps (+normal-mode-map, +insert-mode-map, +motion-mode-map, +sexp-mode-map)
;; are declared in init.el so bind-key calls work before this file loads.

(defvar-local +modal-desired-state nil
  "Buffer-local override for the desired modal state.
Can be 'normal, 'insert, 'motion, or 'sexp.")

(defvar +modal--switching nil
  "Flag to prevent recursion when switching between modal states.")

;; Suppress self-insert in non-insert modes
(suppress-keymap +normal-mode-map t)
(suppress-keymap +motion-mode-map t)
(suppress-keymap +sexp-mode-map t)

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

;;; Custom Replacement Functions

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
(defun +kill-line-insert ()
  "Kill region (or kill-visual-line if no region) then enter insert mode."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-visual-line))
  (+insert-mode 1))

;;;###autoload
(defun +mark-whole-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line)
  (activate-mark))

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

;;; Migrated functions from meow-lisp.el

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
(defun +join-line ()
  (interactive)
  (join-line)
  (unless (derived-mode-p 'yaml-mode)
    (indent-for-tab-command)))

;;;###autoload
;; (defun +back-or-join ()
;;   (interactive)
;;   (cond ((eq last-command this-command)
;;          (+join-line))
;;         ((eq (point) (save-excursion (back-to-indentation) (point)))
;;          (+join-line))
;;         (t
;;          (back-to-indentation)))
;;   (deactivate-mark))

;;;###autoload
(defun +delete ()
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively #'delete-region)
        (indent-for-tab-command))
    (call-interactively #'backward-delete-char-untabify)))

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

;;; Mode-line indicator

(defun +modal--mode-line-indicator ()
  "Return a string indicating the current modal state."
  (cond
   (+sexp-mode "SEXP")
   (+insert-mode "INSERT")
   (+motion-mode "MOTION")
   (+normal-mode "NORMAL")
   (t "")))
