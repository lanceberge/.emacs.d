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
  (save-excursion
    (if (and (eq major-mode 'org-mode)
             (org-in-src-block-p t))
        (+format--org-region nil nil)
      (call-interactively
       (cond ((and (bound-and-true-p +format-with-lsp)
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

(use-package mark-repeat-map
  :ensure nil
  :bind
  (:repeat-map +mark-repeat-map
               ("[" . #'+backward-global-mark)
               ("]" . #'+forward-global-mark))
  (:map +leader3-map
        ("[" . #'+backward-global-mark)
        ("]" . #'+forward-global-mark)))

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
(defun +yas-expand-snippet (snippet-name)
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet snippet-name)))

;;;###autoload
(defun +indent-left (arg)
  (interactive "P")
  (let ((arg (or arg tab-width)))
    (unless (region-active-p)
      (+mark-whole-line))
    (dotimes (_ arg)
      (call-interactively #'indent-rigidly-left))))

;;;###autoload
(defun +indent-right (arg)
  (interactive "P")
  (let ((arg (or arg tab-width)))
    (unless (region-active-p)
      (+mark-whole-line))
    (dotimes (_ arg)
      (call-interactively #'indent-rigidly-right))))

;;;###autoload
(defun beginning-of-indentation-position ()
  (save-excursion
    (back-to-indentation)
    (point)))

;;;###autoload
(defun goto-start-of-region ()
  (if (region-active-p)
      (if (> (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;;;###autoload
(defun goto-end-of-region ()
  (if (region-active-p)
      (if (< (point) (region-beginning))
          (exchange-point-and-mark))
    (user-error "region is not active")))

;; https://stackoverflow.com/questions/2588277/how-can-i-swap-or-replace-multiple-strings-in-code-at-the-same-time
(require 'cl)
(defun parallel-replace (plist &optional start end)
  (interactive
   `(,(cl-loop with input = (read-from-minibuffer "Replace: ")
               with limit = (length input)
               for (item . index) = (read-from-string input 0)
               then (read-from-string input index)
               collect (prin1-to-string item t) until (<= limit index))
     ,@(if (use-region-p) `(,(region-beginning) ,(region-end)))))
  (let* ((alist (cl-loop for (key val . tail) on plist by #'cddr
                         collect (cons key val)))
         (matcher (regexp-opt (mapcar #'car alist) 'words)))
    (save-excursion
      (goto-char (or start (point)))
      (while (re-search-forward matcher (or end (point-max)) t)
        (replace-match (cdr (assoc-string (match-string 0) alist)))))))


;;;###autoload
(defun +rectangle-mode (&optional arg)
  (interactive "p")
  (rectangle-mark-mode)
  (forward-char)
  (next-line (- arg 1)))

;;;###autoload
(defun +insert-newlines-above (arg)
  (interactive "p")
  ;; save excursion didn't work if point is at line-beginning-position
  (let ((col (- (point) (line-beginning-position))))
    (beginning-of-line)
    (dotimes (_ arg)
      (newline))
    (forward-char col)))

;;;###autoload
(defun +insert-newlines-below (arg)
  (interactive "p")
  (save-excursion
    (end-of-line)
    (dotimes (_ arg)
      (newline))))

;;;###autoload
(defun insert-newline-indent ()
  (interactive)
  (save-excursion
    (newline)
    (indent-for-tab-command)))

;;;###autoload
(defun +server-edit ()
  (interactive)
  (save-buffer)
  (server-edit))

;;;###autoload
(defun insert-newline-dwim ()
  (interactive)
  (if (region-active-p)
      (insert-newline-preserving-region)
    (progn
      (newline)
      (indent-for-tab-command))))

;;;###autoload
(defun insert-newline-above-dwim ()
  "split the line above at point"
  (interactive)
  (let ((deactivate-mark nil))
    (if (region-active-p)
        (progn
          (insert-newline-preserving-region)
          (save-excursion
            (next-line)
            (indent-for-tab-command)))
      (progn
        (insert-newline-dwim)
        (drag-stuff-up 1)
        (indent-for-tab-command)))))

;;;###autoload
(defun insert-newline-preserving-region ()
  "Insert a newline at point while preserving the region."
  (let* ((was-active (region-active-p))
         (old-point (point))
         (old-mark (and (region-active-p) (mark t)))
         (deactivate-mark nil)
         (point-at-end (eq old-point (region-end)))
         (region-len (abs (- old-point old-mark))))
    (insert "\n")
    (indent-according-to-mode)
    (when point-at-end
      (goto-char old-point)
      (set-mark (- (point) region-len))
      (activate-mark))))

;;;###autoload
(defun +project-shell-command ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))

;;;###autoload
(defun +source-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;;###autoload
(defun +append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (looking-back ";" nil)
      (insert ";"))))
