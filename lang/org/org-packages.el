;;; -*- lexical-binding: t -*-
(use-package org-ql
  :after org
  :demand t)

(use-package org-agenda
  :ensure nil
  :defer 2.0
  :custom
  (org-agenda-span 14)              ; show 14 days
  (org-agenda-start-on-weekday nil) ; start on today
  (org-agenda-tags-column 0)
  (org-agenda-files '("~/org/projects"))
  (org-agenda-todo-ignore-deadlines 'future)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-inhibit-startup t)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-enforce-todo-dependencies t)
  :bind
  (:map org-agenda-mode-map
        ([remap org-save-all-org-buffers] . #'+org-agenda-save-all-org-buffers))
  (:map +leader-map
        ("oa" . #'org-agenda))
  :config
  (add-hook 'org-blocker-hook #'+org-blocked-by-open-todos-in-file))

(use-package org-habit
  :ensure nil
  :after org-agenda
  :demand t
  :custom
  (org-habit-graph-column 60)
  (org-habit-preceding-days 21)
  (org-habit-following-days 7)
  (org-habit-show-habits-only-for-today nil))

;;;###autoload
(defun +org-srs-add-flashcard (file heading description)
  "Add a flashcard with HEADING and DESCRIPTION to FILE."
  (interactive
   (list (let ((default-directory (expand-file-name "~/org/")))
           (buffer-file-name (call-interactively #'project-find-file)))
         (read-string "Heading: ")
         (read-string "Description: ")))
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (unless (or (bobp) (looking-back "\n\n" nil))
      (insert "\n"))
    (let ((heading-marker (point-marker)))
      (insert "* " heading "\n\n" heading "\n\n** Back\n\n" description "\n")
      (save-excursion
        (goto-char heading-marker)
        (org-id-get-create)
        (org-srs-item-new-interactively 'card))
      (set-marker heading-marker nil))
    (save-buffer)))

(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . (lambda () (+insert-mode 1)))
  :bind
  (:map +leader-map
        ("oc" . #'org-capture)
        ("nf" . #'+org-srs-add-flashcard))
  (:map org-capture-mode-map
        ([remap delete-window] . #'org-capture-kill)
        ([remap +tabspace-kill-buffer-dwim] . #'org-capture-kill)
        ([remap save-buffer] . org-capture-finalize)))

(use-package org-src
  :ensure nil
  :custom
  (org-edit-src-content-indentation 0) ; leading spaces before the #+begin line
  (org-src-preserve-indentation t)     ; don't preserve leading whitespace on export
  (org-adapt-indentation t)
  (org-src-window-setup 'current-window)
  :config
  (setq org-src-tab-acts-natively t))

(use-package org-modern
  :custom
  (org-modern-star 'replace)
  :after org
  :demand t
  :config
  (global-org-modern-mode))

(use-package org-journal
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-file-format "%m%d%Y")
  :bind
  (:map +leader-map
        ("oj" . #'org-journal-new-entry)))

(use-package org-timer
  :ensure nil)

(use-package ob ; org babel
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ox ; org exports
  :ensure nil)

(use-package ol ; org links
  :ensure nil)

(use-package ob-haskell
  :ensure nil
  :commands org-babel-execute:haskell)

(use-package ob-shell
  :ensure nil
  :commands (org-babel-execute:sh org-babel-execute:shell))

(use-package ob-C
  :ensure nil
  :commands org-babel-execute:C)

(use-package ob-R
  :ensure nil
  :commands org-babel-execute:R)

(use-package ob-python
  :ensure nil
  :commands org-babel-execute:python)

(use-package ob-matlab
  :ensure nil
  :commands org-babel-execute:matlab)

(use-package ob-perl
  :ensure nil
  :commands org-babel-execute:perl)

(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t))

(use-package org-super-agenda
  :after org-agenda
  :demand t
  :custom
  (org-super-agenda-groups
   '((:name "Priority A"
            :priority "A"
            :order 0)
     (:name "Habits"
            :habit t
            :order 1)
     (:name "Active Projects"
            :tag "Project"
            :order 3)
     (:name "Life"
            :and (:tag "Project" :tag "Todo")
            :order 5)
     (:name "Low Priority"
            :priority "C"
            :order 90)
     (:auto-category t
                     :order 99)))
  :config
  (require 'org-habit)
  (unless org-super-agenda-mode
    (org-super-agenda-mode 1)))

(use-package org-srs
  :after org
  :hook (org-mode . org-srs-embed-overlay-mode))

(use-package org-srs-review-extras
  :ensure (:type file :main "~/.emacs.d/lisp/org-srs-review-extras.el" :files ("org-srs-review-extras.el"))
  :custom
  (+org-srs-review-scope
   '("~/org/drill"
     "~/code/long_term_learning/days"))
  :bind
  (:map +leader-map
        ("od" . #'+org-srs-review-filetag)))

(use-package org-drill
  :ensure nil
  :custom
  (org-drill-scope
   (append (directory-files-recursively "~/org/drill" "\\.org\\'")
           (directory-files-recursively "~/code/long_term_learning/days" "\\.org\\'"))))

;;;###autoload
(defun +org-drill-filetag (&optional refresh)
  "Run Org Drill for a selected file tag.
With a prefix argument, refresh file tags before prompting."
  (interactive "P")
  (let* ((tags
          (delete-dups
           (mapcan
            (lambda (file)
              (with-current-buffer (find-file-noselect file)
                (when refresh
                  (org-set-regexps-and-options t))
                (mapcar #'substring-no-properties org-file-tags)))
            org-drill-scope)))
         (tag (completing-read "Drill group: " tags nil t)))
    (org-drill nil tag)))

(use-package org-rich-yank
  :bind
  (:map org-mode-map
        ("C-c yy" . org-rich-yank)))
