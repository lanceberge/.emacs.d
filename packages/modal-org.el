;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +modal-org-insert-heading ()
  "Insert an Org heading and enter insert mode."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "^[ \t]*-"))
      (progn
        (end-of-line)
        (org-insert-item))
    (org-insert-heading-respect-content))
  (unless +insert-mode
    (+insert-mode 1)))

;;;###autoload
(defun +modal-org-insert-todo ()
  "Insert an Org TODO heading and enter insert mode."
  (interactive)
  (org-insert-todo-heading-respect-content)
  (unless +insert-mode
    (+insert-mode 1)))

;;;###autoload
(defun +modal-org-insert-heading-above ()
  "Insert an Org heading above and enter insert mode."
  (interactive)
  (+open-above)
  (move-beginning-of-line nil)
  (org-insert-heading 1))

;;;###autoload
(defun +modal-org-insert-todo-above ()
  "Insert an Org TODO heading above and enter insert mode."
  (interactive)
  (+open-above)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 1))

;;;###autoload
(defun +modal-org-insert-below (arg)
  "Open below in Org, creating list items when appropriate."
  (interactive "p")
  (deactivate-mark)
  (end-of-visual-line)
  (if (org-insert-item)
      (+insert-mode 1)
    (+open-below arg)))

;;;###autoload
(defun +modal-org-insert-above ()
  "Open above in Org, creating list items when appropriate."
  (interactive)
  (deactivate-mark)
  (beginning-of-visual-line)
  (if (org-insert-item)
      (+insert-mode 1)
    (+open-above)))

(provide 'modal-org)
