;;; find-char.el --- Character motions -*- lexical-binding: t -*-
(require 'avy)

(defvar +find-char--last-char nil
  "Last character searched for repeating across buffers.")

(defvar +find-char--forward t
  "Direction of the last character search.")

(defvar +find-char--before nil
  "Whether the last search stopped before its match.")

(defvar-local +find-char--avy-char nil
  "Last character searched in this buffer for an Avy jump.")

;;;###autoload
(defun +find-char-find (arg)
  "Find a character in this paragraph, backward when ARG is negative."
  (interactive "P")
  (+find-char--start arg nil))

;;;###autoload
(defun +find-char-goto (arg)
  "Go up to a character in this paragraph, backward when ARG is negative."
  (interactive "P")
  (+find-char--start arg t))

;;;###autoload
(defun +find-char-avy ()
  "Jump with Avy to the last character searched in this buffer."
  (interactive)
  (if +find-char--avy-char
      (avy-goto-char +find-char--avy-char)
    (call-interactively #'avy-goto-char-2)))

;;;###autoload
(defun +find-char-repeat (arg)
  "Repeat the last character search in its original direction."
  (interactive "P")
  (+find-char--repeat arg nil))

;;;###autoload
(defun +find-char-repeat-opposite (arg)
  "Repeat the last character search in the opposite direction."
  (interactive "P")
  (+find-char--repeat arg t))

;;;###autoload
(defun +find-char--start (arg before)
  "Search for a new character with ARG, stopping BEFORE it when non-nil."
  (let* ((count (prefix-numeric-value arg))
         (forward (> count 0)))
    (when (zerop count)
      (user-error "Character search count must be nonzero"))
    (let ((char (read-char)))
      (setq +find-char--last-char char
            +find-char--forward forward
            +find-char--before before)
      (+find-char--move char (abs count) forward before nil)
      (setq +find-char--avy-char char))))

;;;###autoload
(defun +find-char--repeat (arg opposite)
  "Repeat with ARG, reversing the original direction when OPPOSITE."
  (unless +find-char--last-char
    (user-error "No previous character search"))
  (let* ((count (prefix-numeric-value arg))
         (forward (if opposite (not +find-char--forward)
                    +find-char--forward)))
    (when (zerop count)
      (user-error "Character search count must be nonzero"))
    (when (< count 0)
      (setq forward (not forward)))
    (+find-char--move +find-char--last-char (abs count) forward
                      +find-char--before t)))

;;;###autoload
(defun +find-char--move (char count forward before repeated)
  "Search for CHAR COUNT times in a paragraph and move to its match.
FORWARD chooses the direction.  BEFORE stops adjacent to the match.
REPEATED skips matches that would leave point unchanged."
  (let* ((origin (point))
         (bound (save-excursion
                  (if forward (forward-paragraph) (backward-paragraph))
                  (point)))
         (text (char-to-string char))
         (destination
          (save-excursion
            (goto-char (if forward (min bound (1+ origin)) origin))
            (let ((case-fold-search nil)
                  (remaining count)
                  candidate)
              (while (and (> remaining 0)
                          (if forward
                              (search-forward text bound t)
                            (search-backward text bound t)))
                (setq candidate (if forward
                                    (- (point) (if before 2 1))
                                  (+ (point) (if before 1 0))))
                (unless (and repeated (= candidate origin))
                  (setq remaining (1- remaining))))
              (and (zerop remaining) candidate)))))
    (unless destination
      (user-error "Character %c not found" char))
    (goto-char destination)))

(provide 'find-char)
