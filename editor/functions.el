;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +format--org-region (beg end)
  "Reformat the region within BEG and END.
     If nil, BEG and/or END will default to the boundaries of the src block at point."
  (let ((element (org-element-at-point)))
    (save-excursion
      (let* ((block-beg (save-excursion
                          (goto-char (org-babel-where-is-src-block-head element))
                          (line-beginning-position 2)))
             (block-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (skip-chars-backward " \t\n")
                          (line-beginning-position)))
             (beg (if beg (max beg block-beg) block-beg))
             (end (if end (min end block-end) block-end))
             (lang (org-element-property :language element))
             (major-mode (org-src-get-lang-mode lang)))
        (if (eq major-mode 'org-mode)
            (user-error "Cannot reformat an org src block in org-mode")
          (+format/region beg end))))))

;;;###autoload
(defun +format/buffer ()
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive)
  (save-excursion ;; TODO try save-window-excursion
    (if (and (eq major-mode 'org-mode)
             (org-in-src-block-p t))
        (+format--org-region nil nil)
      (call-interactively
       (cond ((and +format-with-lsp
                   (bound-and-true-p lsp-mode)
                   (lsp-feature? "textDocument/formatting"))
              #'lsp-format-buffer)
             (#'format-all-buffer))))))

;;;###autoload
(defun +format/region (beg end)
  "Runs the active formatter on the lines within BEG and END.
   WARNING: this may not work everywhere. It will throw errors if the region
   contains a syntax error in isolation. It is mostly useful for formatting
   snippets or single lines."
  (interactive "rP")
  (if (and (eq major-mode 'org-mode)
           (org-in-src-block-p t))
      (+format--org-region beg end)
    (cond ((and +format-with-lsp
                (bound-and-true-p lsp-mode)
                (lsp-feature? "textDocument/rangeFormatting"))
           (call-interactively #'lsp-format-region))
          ((and +format-with-lsp
                (bound-and-true-p eglot--managed-mode)
                (eglot--server-capable :documentRangeFormattingProvider))
           (call-interactively #'eglot-format))
          ((save-restriction
             (narrow-to-region beg end)
             (let ((+format-region-p t))
               (+format/buffer)))))))

(defun +dired/edit ()
  "stay in normal mode to edit dired file names"
  (interactive)
  (dired-toggle-read-only)
  (evil-normal-state)
  (evil-forward-char))

(defun +dired/up-dir ()
  "navigate up a directory in dired in the same buffer"
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun read-file-contents (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; TODO rename all my/ to +
;;;###autoload
(defun my/save-and-kill-buffer ()
  "save and kill buffer"
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;;;###autoload
(defun my/split-line-below ()
  "split the line below at point"
  (interactive)
  (newline)
  (indent-according-to-mode))

;;;###autoload
(defun my/split-line-above ()
  "split the line above at point"
  (interactive)
  (my/split-line-below)
  (move-text-up))

;;;###autoload
(defun my/kill-window-and-buffer ()
  "kill window and buffer"
  (interactive)
  (kill-this-buffer)
  (evil-quit))

;;;###autoload
(defun my/append-semicolon()
  "append a semicolon to the end of the line"
  (interactive)
  (save-excursion
    (call-interactively 'move-end-of-line)
    (insert ";")))

(defun balanced-parens-p ()
  "Return `t' if parentheses are balanced; otherwise `nil'."
  (condition-case nil
      (progn
        (check-parens)
        t)
    (error nil)))
