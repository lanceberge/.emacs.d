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

(defun +harpoon--get-name (&optional name)
  (interactive)
  (let ((project-prefix (or (project-root (project-current t)) "nil"))
        (suffix (if name
                    (format ":%s" name)
                  "")))
    (format "%s%s" project-prefix suffix)))

(defun +harpoon-bookmark (name)
  (interactive "p")
  (bookmark-set (+harpoon--get-name name)))

(defun +harpoon-goto (name)
  (interactive "p")
  (let ((bookmark-name (+harpoon--get-name name)))
    (condition-case err
        (bookmark-jump bookmark-name)
      (error
       (+harpoon-bookmark name)))))
