;;; -*- lexical-binding: t -*-

(defvar +surround-pairs '((?r . ("(" . ")"))
                          (?s . ("[" . "]"))
                          (?g . ("\"" . "\""))))

(use-package +surround
  :ensure nil
  :hook
  (org-mode . +setup-org-pairs)
  :bind
  (:map meow-normal-state-keymap
        ("S" . #'+surround)))

(defun +setup-org-pairs ()
  (add-to-list '+surround-pairs '(?e . ("#+BEGIN_SRC elixir\n" . "\n#+END_SRC\n"))))

;;;###autoload
(defun +surround (char)
  (interactive "c")
  (cond ((eq char ?d)
         (+surround-delete))
        ((eq char ?x)
         (call-interactively #'+surround-change))
        (t
         (+surround-surround char))))

;;;###autoload
(defun +surround-surround (char)
  (interactive "c")
  (save-excursion
    (+surround--select-region)
    (let* ((pairs (+surround--get-pairs char))
           (open-string (car pairs))
           (close-string (cdr pairs))
           (start (region-beginning))
           (end (region-end)))
      (when (> end start)
        (goto-char end)
        (insert close-string)
        (goto-char start)
        (insert open-string)
        (keyboard-quit)))))

;;;###autoload
(defun +surround-delete ()
  (interactive)
  (save-excursion
    (+surround--select-region)
    (+surround-delete)))

;;;###autoload
(defun +surround-change (char)
  (interactive "cReplace:")
  (save-excursion
    (+surround--select-region)
    (let* ((start (region-beginning))
           (end (region-end))
           (pairs (+surround--get-pairs char))
           (open-string (car pairs))
           (close-string (cdr pairs)))
      (when (> end start)
        (goto-char end)
        (backward-char 1)
        (delete-char 1)
        (insert close-string)
        (goto-char start)
        (delete-char 1)
        (insert open-string)
        (keyboard-quit)))))

;;;###autoload
(defun +surround-delete ()
  (interactive)
  (save-excursion
    (+surround--select-region)
    (let* ((start (region-beginning))
           (end (region-end)))
      (when (> end start)
        (goto-char end)
        (backward-char 1)
        (delete-char 1)
        (goto-char start)
        (delete-char 1)
        (keyboard-quit)))))

;;;###autoload
(defun +surround--get-pairs (char)
  (let* ((pair (cdr (assoc char +surround-pairs)))
         (default-pair (char-to-string char))
         (open-string (or (car pair) default-pair))
         (close-string (or (cdr pair) default-pair)))
    (cons open-string close-string)))

;;;###autoload
(defun +surround--select-region ()
  (if (region-active-p)
      (+surround--narrow-to-non-whitespace)
    (require 'expand-region)
    (let ((original-expand-list er/try-expand-list))
      (setq-local er/try-expand-list '(er/mark-inside-quotes
                                       er/mark-inside-pairs
                                       er/mark-outside-quotes
                                       er/mark-outside-pairs))
      (er/expand-region 2)
      (setq er/try-expand-list original-expand-list))))

;;;###autoload
(defun +surround--narrow-to-non-whitespace ()
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (non-whitespace-start (save-excursion
                                     (goto-char start)
                                     (skip-chars-forward " \t\n")
                                     (point)))
             (non-whitespace-end (save-excursion
                                   (goto-char end)
                                   (skip-chars-backward " \t\n")
                                   (point))))
        (goto-char non-whitespace-start)
        (set-mark non-whitespace-end)
        (activate-mark))
    (user-error "No active region")))
