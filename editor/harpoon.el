(use-package emacs
  :ensure nil
  :config
  ;; loops don't work with this damn macro...
  (my-leader-def
    "SPC 1" (lambda () (interactive) (+harpoon-bookmark "1"))
    "SPC 2" (lambda () (interactive) (+harpoon-bookmark "2"))
    "SPC 3" (lambda () (interactive) (+harpoon-bookmark "3"))
    "SPC 4" (lambda () (interactive) (+harpoon-bookmark "4"))
    "SPC 5" (lambda () (interactive) (+harpoon-bookmark "5"))
    "SPC 6" (lambda () (interactive) (+harpoon-bookmark "6"))
    "SPC 7" (lambda () (interactive) (+harpoon-bookmark "7"))
    "SPC 8" (lambda () (interactive) (+harpoon-bookmark "8"))
    "SPC 9" (lambda () (interactive) (+harpoon-bookmark "9"))

    "1" (lambda () (interactive) (+harpoon-goto "1"))
    "2" (lambda () (interactive) (+harpoon-goto "2"))
    "3" (lambda () (interactive) (+harpoon-goto "3"))
    "4" (lambda () (interactive) (+harpoon-goto "4"))
    "5" (lambda () (interactive) (+harpoon-goto "5"))
    "6" (lambda () (interactive) (+harpoon-goto "6"))
    "7" (lambda () (interactive) (+harpoon-goto "7"))
    "8" (lambda () (interactive) (+harpoon-goto "8"))
    "9" (lambda () (interactive) (+harpoon-goto "9"))))

(defun +harpoon--get-name (name)
  (interactive)
  (let ((project-prefix (or (project-root (project-current t)) "nil")))
    (format "%s:%s" project-prefix name)))

(defun +harpoon-bookmark (name)
  (interactive "p")
  (bookmark-set (+harpoon--get-name name)))

(defun +harpoon-goto (name)
  (interactive "p")
  (bookmark-jump (+harpoon--get-name name)))
