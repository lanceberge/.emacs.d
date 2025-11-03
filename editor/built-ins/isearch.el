;;; -*- lexical-binding: t -*-
(use-package isearch
  :ensure nil
  :hook (isearch-mode-end . +isearch-exit-at-start)
  :bind
  (:repeat-map isearch-repeat-map
               ("n" . #'isearch-repeat-forward)
               ("p" . #'isearch-repeat-backward))
  (:map meow-normal-state-keymap
        ("nn" . #'isearch-repeat-forward)
        ("np" . #'isearch-repeat-backward))
  (:map isearch-mode-map
        ("C-j" . #'isearch-repeat-forward)
        ("C-k" . #'isearch-repeat-backward)
        ("M-r" . nil)
        ("M-r l" . #'+consult-line)
        ("M-r L" . #'+consult-line-multi)
        ("M-r s" . #'+isearch-consult-ripgrep)
        ("M-/" . #'+consult-line)
        ("C-M-/" . #'+consult-line-multi)
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
(defun +isearch-consult-ripgrep ()
  (interactive)
  (isearch-done)
  (consult-ripgrep nil isearch-string))

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
(defun +keyboard-quit ()
  (interactive)
  (save-excursion
    (lazy-highlight-cleanup)
    (isearch-exit))
  (if (eq last-command this-command)
      (call-interactively #'abort-recursive-edit))
  (keyboard-quit))
