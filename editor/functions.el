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
  (save-excursion ;; TODO try treesitter-save-excursion
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

;;;###autoload
(defun +save-and-kill-buffer ()
  "save and kill buffer"
  (interactive)
  (save-buffer)
  (kill-current-buffer))

;;;###autoload
(defun +split-line-below ()
  "split the line below at point"
  (interactive)
  (newline)
  (indent-according-to-mode))

;;;###autoload
(defun +split-line-above ()
  "split the line above at point"
  (interactive)
  (+split-line-below)
  (transpose-lines 1)
  (forward-line -2))

;;;###autoload
(defun +kill-window-and-buffer ()
  "kill window and buffer"
  (interactive)
  (kill-this-buffer)
  (evil-quit))

;;;###autoload
(defun balanced-parens-p ()
  "Return `t' if parentheses are balanced; otherwise `nil'."
  (condition-case nil
      (progn
        (check-parens)
        t)
    (error nil)))

;;;###autoload
(defun +flycheck-list-errors ()
  "Display the Flycheck error list and set its window height to take up 1/3 of the frame."
  (interactive)
  (flycheck-list-errors)
  (let ((error-window (get-buffer-window "*Flycheck errors*" t)))
    (when error-window
      (with-selected-window error-window
        (let ((window-height (round (* 0.33 (frame-height)))))
          (enlarge-window (- window-height (window-height))))))))

;; https://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring
;;;###autoload
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

;;;###autoload
(defun +push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

;;;###autoload
(defun +backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (+push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

;;;###autoload
(defun +forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (+push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

;;;###autoload
(defun +expand-snippet (snippet-name)
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet snippet-name)))

;;;###autoload
(defun +project-switch-and-rg ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'consult-ripgrep)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-find-file ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'project-find-file)
  (call-interactively 'project-switch-project))

;;;###autoload
(defun +project-switch-and-magit-status ()
  "Temporarily sets projectile-switch-project-action to counsel-rg and then switches project with Projectile."
  (interactive)
  (setq project-switch-commands #'magit-project-status)
  (call-interactively 'project-switch-project))
