;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +org/insert-heading ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-heading-respect-content)
  (unless meow-insert-mode
    (meow-insert-mode 1)))

;;;###autoload
(defun +org/insert-todo ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-todo-heading-respect-content)
  (unless meow-insert-mode
    (meow-insert-mode 1)))

;;;###autoload
(defun +org/insert-heading-above ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-heading-respect-content)
  (unless meow-insert-mode
    (meow-insert-mode 1)))

;;;###autoload
(defun +org/insert-heading-above ()
  "insert an org heading above and jump into insert mode"
  (interactive)
  (meow-open-above)
  (move-beginning-of-line nil)
  (org-insert-heading 1))

;;;###autoload
(defun +org/insert-todo-above ()
  "insert an org todo above and jump into insert mode"
  (interactive)
  (meow-open-above)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 1))

;;;###autoload
(defun +org-indent ()
  "indent in major mode - org-src-tab-acts-natively gave me issues"
  (interactive)
  (if (org-in-src-block-p)
      (save-window-excursion
        (progn
          (org-edit-src-code)
          (call-interactively #'indent-for-tab-command)
          (org-edit-src-exit)))))

;;;###autoload
(defun +org-agenda-filter-by-tag ()
  "indent in major mode - org-src-tab-acts-natively gave me issues"
  (interactive)
  (org-agenda-filter-by-tag t ?\t))

;;;###autoload
(defun +org-insert-below ()
  (interactive)
  (deactivate-mark)
  (end-of-visual-line)
  (if (org-insert-item)
      (meow-insert)
    (+meow-open-below)))

;;;###autoload
(defun +org-insert-above ()
  (interactive)
  (deactivate-mark)
  (beginning-of-visual-line)
  (if (org-insert-item)
      (meow-insert)
    (meow-open-above)))

;;;###autoload
(defun +org-drag-stuff-up (n)
  (interactive "p")
  (if (org-at-heading-p)
      (org-move-subtree-up n)
    (drag-stuff-up n)))

;;;###autoload
(defun +org-drag-stuff-down (n)
  (interactive "p")
  (if (org-at-heading-p)
      (org-move-subtree-down n)
    (drag-stuff-down n)))

;;;###autoload
(defun +org-cycle ()
  (interactive)
  (if (save-excursion
        (beginning-of-visual-line)
        (org-at-heading-p))
      (outline-toggle-children)
    (org-cycle)))

;;;###autoload
(defun +org-metaleft-dwim (arg)
  (interactive "p")
  (if (not (region-active-p))
      (call-interactively #'org-metaleft)
    (+drag-stuff-left-dwim arg)))

;;;###autoload
(defun +org-metaright-dwim (arg)
  (interactive "p")
  (if (not (region-active-p))
      (call-interactively #'org-metaright)
    (+drag-stuff-right-dwim arg)))
