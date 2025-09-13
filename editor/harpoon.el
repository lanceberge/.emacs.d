;;; -*- lexical-binding: t -*-
(dotimes (i 10)
  (let ((num (number-to-string i)))
    (define-key +leader-map num
                `(lambda ()
                   (interactive)
                   (+harpoon-goto ,num)))
    (define-key +leader-map (kbd (format "SPC %s" num))
                `(lambda ()
                   (interactive)
                   (+harpoon-bookmark ,num)))))

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
