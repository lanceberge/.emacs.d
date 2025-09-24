;;; -*- lexical-binding: t -*-
(use-package isearch
  :ensure nil
  :hook (isearch-mode-end . +isearch-exit-at-start)
  :bind
  (:map meow-normal-state-keymap
        ("n" . #'isearch-repeat-forward)
        ("N" . #'isearch-repeat-backward)
        ("/" . #'isearch-forward)
        ("?" . #'isearch-backward))
  (:map isearch-mode-map
        ("C-j" . #'isearch-repeat-forward)
        ("C-k" . #'isearch-repeat-backward)
        ("M-/" . #'+isearch-consult-line)
        ("M-j" . #'avy-isearch))
  :config
  (setq search-nonincremental-instead nil)) ; don't cancel isearches with searches

;;;###autoload
(defun +isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (unless (or isearch-mode-end-hook-quit
              (bound-and-true-p isearch-suspended)
              (not isearch-forward)
              (not isearch-other-end)
              (and (boundp 'avy-command)
                   (eq avy-command 'avy-isearch)))
    (goto-char isearch-other-end)))

;;;###autoload
(defun +isearch-consult-line ()
  "Exit isearch and run consult-line with the current search string."
  (interactive)
  (let ((search-string (if isearch-regexp
                           isearch-string
                         isearch-string)))
    (isearch-done)
    (consult-line search-string)))

;;;###autoload
(defun +isearch-update-last-search (search-string)
  "Update isearch state to use SEARCH-STRING as the last search to be used by isearch-repeat."
  (when search-string
    (setq isearch-string search-string)
    (isearch-update-ring search-string isearch-regexp)
    (setq isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string ""))
    (setq isearch-case-fold-search isearch-last-case-fold-search)
    (setq isearch-success t)))

;;;###autoload
(defun +isearch-clear-highlighting ()
  (interactive)
  (save-excursion
    (lazy-highlight-cleanup)
    (isearch-exit))
  (keyboard-quit))
