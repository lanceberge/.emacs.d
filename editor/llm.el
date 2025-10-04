;;; -*- lexical-binding: t -*-
(use-package gptel
  :demand t
  :custom
  (gptel-model 'claude-3-5-sonnet-20240620)
  (gptel-default-mode 'org-mode)
  :bind
  ("C-c C-l". #'+gptel-project-clear-buffer)
  (:map gptel-mode-map
        ("C-c C-l". #'+gptel-project-clear-buffer)
        ("C-RET" . #'+gptel-send)
        ([remap +org/insert-heading] . #'+gptel-send)
        ([remap gptel-send] . #'+gptel-send)
        ;; ([remap +org/insert-heading] . #'gptel-send)
        ;; ([remap org-return] . #'+gptel-send)
        ("S-<return>" . #'newline))
  :bind
  (:map +leader2-map
        ("gt" . +gptel-project)
        ("sc" . #'+gptel-project-add-context)
        ("s'" . #'+gptel-project-clear-buffer))
  :config
  (setq
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key)))

;;;###autoload
(defun +gptel-send ()
  (interactive)
  (end-of-buffer)
  (gptel-send))

;;;###autoload
(defun gptel-api-key ()
  (read-file-contents "~/secrets/claude_key"))

;;;###autoload
(defun +gptel-project-buffer-name ()
  (format "gptel-%s" (+current-proj-tab-name)))

;;;###autoload
(defun +gptel-project ()
  (interactive)
  (let* ((gptel-buffer-name (+gptel-project-buffer-name))
         (gptel-buffer (get-buffer gptel-buffer-name)))
    (if gptel-buffer
        (switch-to-buffer gptel-buffer)
      (switch-to-buffer (gptel gptel-buffer-name)))))

;;;###autoload
(defun +gptel-project-clear-buffer ()
  (interactive)
  (let ((gptel-proj-buffer (+gptel-project-buffer-name)))
    (unless (eq (current-buffer) (get-buffer gptel-proj-buffer))
      (+gptel-project))
    (with-current-buffer gptel-proj-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

;;;###autoload
(defun +gptel-project-add-context ()
  (interactive)
  (let ((content (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
        (code-buffer-language
         (string-trim-right
          (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (+gptel-project)
    (with-current-buffer (+gptel-project-buffer-name)
      (goto-char (point-max))
      (insert "\n")
      (let ((src-pattern
             (cond
              ((derived-mode-p 'markdown-mode)
               "```%s\n%s\n```")
              ((derived-mode-p 'org-mode)
               "#+begin_src %s\n%s\n#+end_src")
              (t "%s%s"))))
        (insert (format src-pattern code-buffer-language content))))))

(use-package elysium
  :bind
  (:map +leader2-map
        ("sq" . #'elysium-query)
        ("so" . #'elysium)
        ("sw" . #'elysium-toggle-window))
  (:map +leader-map
        ("m SPC" . #'elysium-keep-all-suggested-changes)
        ("m-" . #'elysium-discard-all-suggested-changes)))
