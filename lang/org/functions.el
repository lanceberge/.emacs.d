;;;###autoload
(defun +org/insert-subheading ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (evil-append-line 1)
  (org-insert-subheading 1))

;;;###autoload
(defun +org/insert-heading ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-heading-respect-content)
  (evil-insert 1))

;;;###autoload
(defun +org/insert-todo ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-insert 1))

;;;###autoload
(defun +org/insert-heading-above ()
  "insert a subheading in org mode and go to insert mode"
  (interactive)
  (org-insert-heading-respect-content)
  (evil-insert 1))

;;;###autoload
(defun +org/insert-heading-above ()
  "insert an org heading above and jump into insert mode"
  (interactive)
  (evil-append-line 1)
  (move-beginning-of-line nil)
  (org-insert-heading))

;;;###autoload
(defun +org/insert-todo-above ()
  "insert an org todo above and jump into insert mode"
  (interactive)
  (evil-append-line 1)
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
