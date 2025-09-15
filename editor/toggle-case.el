(use-package +toggle-case
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("~" . #'+toggle-region-case-dwim)))

(defvar +toggle-last nil)
(put 'upcase-region 'disabled nil)

(defun +toggle-region-case-dwim ()
  (interactive)
  (if (region-active-p)
      (xah-toggle-letter-case)
    (+toggle--letter-case)))

(defun +toggle--letter-case ()
  (if (region-active-p)
      (user-error "Region should not be active"))
  (let ((char-at-point (char-after (point))))
    (set-mark (point))
    (forward-char)
    (activate-mark)
    (cond ((and (>= char-at-point ?A) (<= char-at-point ?Z))
           (call-interactively #'downcase-region))
          ((and (>= char-at-point ?a) (<= char-at-point ?z))
           (call-interactively #'upcase-region))))
  (deactivate-mark))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Created: 2020-06-26
Version: 2024-06-17"
  (interactive)
  (let ((deactivate-mark nil) xbeg xend)
    (if (region-active-p)
        (setq xbeg (region-beginning) xend (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq xbeg (point))
        (skip-chars-forward "[:alnum:]")
        (setq xend (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xbeg xend)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xbeg xend)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xbeg xend)
      (put this-command 'state 0)))))
