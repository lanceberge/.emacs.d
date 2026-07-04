;;; -*- lexical-binding: t -*-

(require 'isearch)
(require 'consult)

(defvar +isearch--exit-at-end nil
  "When non-nil, `+isearch-exit-at-start' should not move point.")

;;;###autoload
(defun +isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (unless (or isearch-mode-end-hook-quit
              (bound-and-true-p isearch-suspended)
              +isearch--exit-at-end
              (not isearch-forward)
              (not isearch-other-end)
              (and (boundp 'avy-command)
                   (eq avy-command 'avy-isearch)))
    (goto-char isearch-other-end)))

;;;###autoload
(defun +isearch-exit-at-end ()
  "Exit search at the end of the current match."
  (interactive)
  (let ((+isearch--exit-at-end t))
    (isearch-done)))

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
  (if (or (eq last-command this-command) (eq last-command #'+keyboard-quit-normal))
      (call-interactively #'abort-recursive-edit))
  (call-interactively #'keyboard-quit))

(provide 'isearch-extras)
