;;; -*- lexical-binding: t -*-
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

;;;###autoload
(defun +org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;;;###autoload
(defun +org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;;###autoload
(defun +org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (+org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

;;;###autoload
(defun +org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (append (+org-roam-list-notes-by-tag "Project") org-agenda-files)))

;;;###autoload
(defun +org-roam-project-finalize-hook ()
  (remove-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)

  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name))))
  )

;;;###autoload
(defun +org-roam-add-todo ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (+org-roam-filter-by-tag "Project"))
   :templates '(("p" "project" plain "** TODO %?"
                 :if-new
                 (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                ("Tasks"))))))
