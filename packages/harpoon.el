;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +harpoon-bookmark (n)
  "Take a number `n' and create a formatted project-local bookmark based on `n'"
  (interactive "p")
  (bookmark-set (+harpoon--get-name n)))

;;;###autoload
(defun +harpoon-goto (name)
  "Goto a bookmark if it exists otherwise create one."
  (interactive "p")
  (let ((bookmark-name (+harpoon--get-name name)))
    (condition-case err
        (bookmark-jump bookmark-name)
      (error
       (+harpoon-bookmark name)))))

;;;###autoload
(defun +harpoon--get-name (&optional name)
  (interactive)
  (let ((project-prefix (or (project-root (project-current nil)) "nil"))
        (suffix (if name
                    (format ":%s" name)
                  "")))
    (format "%s%s" project-prefix suffix)))

;;;###autoload
(defun +consult-harpoon-bookmarks ()
  "Jump to a bookmark in the current project with consult preview."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (insert (+harpoon--get-name)))
    (call-interactively #'consult-bookmark)))

(provide 'harpoon)
