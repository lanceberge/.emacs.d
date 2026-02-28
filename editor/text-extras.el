;;; -*- lexical-binding: t -*-
(defun pipe-region (start end command)
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
  (interactive (append
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))
                (list (read-shell-command "Pipe through: "))))
  (let ((exit-status (call-shell-region start end command t t)))
    (unless (equal 0 exit-status)
      (let ((error-msg (string-trim-right (buffer-substring (mark) (point)))))
        (undo)
        (cond
         ((null exit-status)
          (message "Unknown error"))
         ((stringp exit-status)
          (message "Signal %s" exit-status))
         (t
          (message "[%d] %s" exit-status error-msg)))))))

(use-package text-extras
  :ensure nil
  :bind
  (:map +normal-mode-map
        ("|" . #'pipe-region))
  (:map +leader-map
        ("u" . #'text-to-clipboard)))


(defun text-to-clipboard ()
  "Pop up a temporary buffer to collect text to send to the clipboard.
The pop up buffer is in `markdown-mode' and uses the TeX input
method.  Use \\<text-to-clipboard-minor-mode-map>\\[text-to-clipboard--done] to send the buffer contents to the clipboard
and quit the window, killing the buffer.

If the region is active, use the region as the initial contents
for the pop up buffer."
  (interactive)
  (let ((region (when (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))
    (pop-to-buffer (generate-new-buffer "*clipboard*"))
    (when region (insert region)))
  (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
  (text-to-clipboard-minor-mode))
(defun text-to-clipboard--done ()
  "Copy buffer contents to clipboard and quit window."
  (interactive)
  (gui-set-selection
   'CLIPBOARD
   (buffer-substring-no-properties (point-min) (point-max)))
  (quit-window :kill))

(defvar-keymap text-to-clipboard-minor-mode-map
  "C-c C-c" #'text-to-clipboard--done)

(define-minor-mode text-to-clipboard-minor-mode
  "Minor mode binding a key to quit window and copy buffer to clipboard.")

(declare-function org-edit-src-code 'org-src)
(declare-function org-edit-src-exit 'org-src)
(declare-function TeX-narrow-to-group 'tex)
(declare-function LaTeX-narrow-to-environment 'latex)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((and (bound-and-true-p org-src-mode) (not p))
         (org-edit-src-exit))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-edit-src-code))
             (ignore-errors (org-narrow-to-block))
             (org-narrow-to-subtree)))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        ((derived-mode-p 'tex-mode)
         (TeX-narrow-to-group))
        (t (narrow-to-defun))))

(defun narrow-to-point ()
  "Narrow to point, useful for yanking a rectangle."
  (interactive)
  (narrow-to-region (point) (point)))

(defun narrow-to-sexp ()
  "Narrow to sexp containing point."
  (interactive)
  (narrow-to-region
   (save-excursion (up-list -1 t t) (point))
   (save-excursion (up-list +1 t t) (point))))
