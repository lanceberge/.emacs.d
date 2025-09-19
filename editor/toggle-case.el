(use-package evil-numbers
  :bind
  (:map meow-normal-state-keymap
        ("M-`" . #'evil-numbers/inc-at-pt)
        ("~" . #'evil-numbers/dec-at-pt)
        ("M-~" . #'evil-numbers/dec-at-pt)))

(use-package +toggle-case
  :ensure nil
  :bind
  (:map meow-normal-state-keymap
        ("`" . #'+toggle-region-or-number-dwim)))

(put 'upcase-region 'disabled nil)

;;;###autoload
(defun +toggle-region-or-number-dwim (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (xah-toggle-letter-case)
    (+toggle-number-or-char arg)))

;;;###autoload
(defun +toggle-number-or-char (&optional arg)
  "Search to the first char or positive/negative number. If it's a char,
toggle the case. Otherwise, increment the number."
  (interactive "p")
  (if (region-active-p)
      (user-error "Region should not be active"))
  (let ((line-end (line-end-position)))
    (while (and (< (point) line-end)
                (not (or (looking-at "[a-zA-Z0-9]")
                         (looking-at "-[1-9]"))))
      (forward-char 1)))
  (let ((case-fold-search nil))
    (if (or (looking-at "[0-9]")
            (looking-at "-[1-9]"))
        (progn
          (require 'evil-numbers)
          (call-interactively #'evil-numbers/inc-at-pt))
      (+toggle--letter-case arg))))

;;;###autoload
(defun +toggle-region-case-dwim ()
  (interactive)
  (if (region-active-p)
      (xah-toggle-letter-case)
    (+toggle--letter-case)))

;;;###autoload
(defun +toggle--letter-case (arg)
  (if (region-active-p)
      (user-error "Region should not be active"))
  (let ((line-end (line-end-position)))
    (while (and (< (point) line-end)
                (not (looking-at "[a-zA-Z]")))
      (forward-char 1)))
  (let ((case-fold-search nil))
    (set-mark (point))
    (cond ((looking-at "[A-Z]")
           (forward-char arg)
           (call-interactively #'downcase-region))
          ((looking-at "[a-z]")
           (forward-char arg)
           (call-interactively #'upcase-region)))))

;;;###autoload
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
