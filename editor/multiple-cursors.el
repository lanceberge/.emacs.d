;;; -*- lexical-binding: t -*-

(use-package multiple-cursors
  :demand t
  :custom
  (mc/always-run-for-all t)
  :config
  (mc/load-lists)
  ;; TODO figure out how to have all minibuffer commands only run once ideally
  (dolist (command '(+insert-mode +normal-mode
                                  embark-act embark-insert
                                  lasgun-make-multiple-cursors))
    (add-to-list 'mc/cmds-to-run-once command))
  :bind
  (:map +leader-map
        ("mm" . #'mc/mark-all-dwim)
        ("m SPC" . #'mc/mark-pop))
  (:map mc/keymap
        ("g" . #'+mc/keyboard-quit)
        ("<return>" . nil))
  :config
  (defun +mc/keyboard-quit ()
    (interactive)
    (if +normal-mode
        (progn
          (mc/keyboard-quit)
          (lasgun-clear-lasgun-mark-ring))
      (call-interactively #'self-insert-command))))

(use-package selected-mc
  :ensure nil
  :no-require t
  :demand t
  :after (multiple-cursors selected)
  :bind
  (:map selected-keymap
        ("c" . #'mc/edit-lines)

        ("]" . #'mc/mark-next-like-this)
        ("[" . #'mc/mark-previous-like-this)

        ("," . #'mc/skip-to-previous-like-this)
        ("." . #'mc/skip-to-next-like-this)

        ("(" . #'mc/unmark-previous-like-this)
        (")" . #'mc/unmark-next-like-this)))

(use-package vr/multiple-cursors
  :ensure nil
  :no-require t
  :bind
  (:map +leader-map
        ("mr" . #'vr/mc-mark)))

(use-package mc-extras
  :bind
  (:map +leader-map
        ("mf" . #'mc/freeze-fake-cursors-dwim))
  (:map mc/keymap
        ("G" . #'+mc-freeze)))

;;;###autoload
(defun +mc-freeze ()
  (interactive)
  (if +normal-mode
      (mc/freeze-fake-cursors-dwim)
    (call-interactively #'self-insert-command)))

;;;###autoload
(defun +lasgun-mark-word-0-cursor ()
  "Select a word with Lasgun and immediately add a cursor there."
  (interactive)
  (let ((lasgun-pop-before-make-multiple-cursors nil))
    (call-interactively #'lasgun-mark-word-0)
    (lasgun-make-multiple-cursors nil)))

;;;###autoload
(defun +lasgun-mark-char-2-cursor ()
  "Select two characters with Lasgun and immediately add a cursor there."
  (interactive)
  (let ((lasgun-pop-before-make-multiple-cursors nil))
    (call-interactively #'lasgun-mark-char-2)
    (lasgun-make-multiple-cursors nil)))

(use-package lasgun
  :demand t
  :after multiple-cursors
  :ensure (:host github :repo "aatmunbaxi/lasgun.el")
  :config
  (dolist (command '(lasgun-mark-word-0
                     lasgun-mark-char-2
                     lasgun-embark-act-all
                     +lasgun-mark-word-0-cursor
                     +lasgun-mark-char-2-cursor))
    (add-to-list 'mc/cmds-to-run-once command))
  :bind
  (:repeat-map +lasgun-repeat-map
               ("'" . #'+lasgun-mark-word-0-cursor)
               (";" . #'+lasgun-mark-char-2-cursor))
  (:map +leader-map
        ("m'" . #'+lasgun-mark-word-0-cursor)
        ("m;" . #'+lasgun-mark-char-2-cursor)
        ("me" . #'lasgun-make-multiple-cursors))
  (:map mc/keymap
        ("A" . #'lasgun-embark-act-all)
        ("'" . #'+lasgun-mark-word-0-cursor)
        (";" . #'+lasgun-mark-char-2-cursor)))
