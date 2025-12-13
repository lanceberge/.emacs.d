;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +org-find-file ()
  (interactive)
  (+consult--buffer-in-dir "~/org-roam/"))

;;;###autoload
(defun +org/insert-heading ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "^[ \t]*-"))
      (progn
        (end-of-line)
        (org-insert-item))
    (org-insert-heading-respect-content))
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
(defun +org-insert-below (arg)
  (interactive "p")
  (deactivate-mark)
  (end-of-visual-line)
  (if (org-insert-item)
      (meow-insert)
    (+meow-open-below arg)))

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
  (cond ((full-line-region-p)
         (drag-stuff-up n))
        ((org-at-heading-p)
         (org-move-subtree-up n))
        (t
         (drag-stuff-up n))))

;;;###autoload
(defun +org-drag-stuff-down (n)
  (interactive "p")
  (cond ((full-line-region-p)
         (drag-stuff-down n))
        ((org-at-heading-p)
         (org-move-subtree-down n))
        (t
         (drag-stuff-down n))))

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
  (interactive "P")
  (if (not (region-active-p))
      (org-metaleft arg)
    (+drag-stuff-left-dwim (* 2 (or arg 1)))))

;;;###autoload
(defun +org-metaright-dwim (arg)
  (interactive "P")
  (if (not (region-active-p))
      (org-metaright arg)
    (+drag-stuff-right-dwim (* 2 (or arg 1)))))

;;;###autoload
(defun +org--has-filetag-p (tag)
  (and (member tag (+org--get-filetags)) t))

;;;###autoload
(defun +org--get-filetags ()
  "Return a list of filetags from the current buffer."
  (and (eq major-mode 'org-mode)
       (let ((filetags (car (org-collect-keywords '("FILETAGS")))))
         (when filetags
           (split-string (cadr filetags) ":" t "\\s-*")))))
