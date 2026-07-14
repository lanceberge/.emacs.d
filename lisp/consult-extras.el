;;; -*- lexical-binding: t -*-

(require 'consult)
(require 'cl-lib)
(require 'project)
(require 'isearch-extras)
(require 'grep-extras)

(declare-function embark-export "embark")
(declare-function embark-export-dired "embark")
(defvar embark-exporters-alist)

;;;###autoload
(defun +consult-project-file-here ()
  "Find a project file under `default-directory'."
  (interactive)
  (+consult--project-file-at-dir default-directory))

;;;###autoload
(defun +consult-project-file-source (dir &optional name)
  "Return a Consult project file source restricted to DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        (name (or name "Project File Here")))
    `( :name     ,name
       :narrow   ?f
       :category file
       :face     consult-file
       :history  file-name-history
       :state    ,#'consult--file-state
       :new
       ,(lambda (file)
          (consult--file-action file))
       :enabled
       ,(lambda ()
          (project-current nil dir))
       :items
       ,(lambda ()
          (when-let* ((project (project-current nil dir))
                      (root (project-root project)))
            (let (items)
              (dolist (file (project-files project) (nreverse items))
                (let ((abs (expand-file-name file root)))
                  (when (file-in-directory-p abs dir)
                    (push
                     (cons (file-relative-name abs dir) abs)
                     items))))))))))

;;;###autoload
(defun +consult--project-file-at-dir (dir &optional name)
  "Find a project file under DIR."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (consult--with-project
      (consult-buffer
       (list (+consult-project-file-source default-directory name))))))

;;;###autoload
(defun +consult-preview-tramp-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from consult preview.
Excludes Tramp buffers so preview never opens a remote connection."
  (when-let ((dir (buffer-local-value 'default-directory buffer)))
    (file-remote-p dir)))

;;;###autoload
(defun +consult-yank-replace-region (&optional kill)
  "Replace the active region with a kill-ring entry selected by Consult.
With prefix argument KILL, save the replaced text in the kill ring.
If selection is aborted, restore the original region text."
  (interactive "P")
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((replaced-text (when kill
                         (filter-buffer-substring (region-beginning)
                                                  (region-end)))))
    (atomic-change-group
      (delete-active-region)
      (let ((current-prefix-arg nil))
        (call-interactively #'consult-yank-pop))
      (when kill
        (kill-new replaced-text)))))

;;;###autoload
(defun +consult-ripgrep-here ()
  (interactive)
  (consult-ripgrep default-directory nil))

;;;###autoload
(defun +consult-grep-export-dired ()
  "Export current Consult grep candidates to a Dired buffer."
  (interactive)
  (let ((embark-exporters-alist
         (cons '(consult-grep . +consult-grep--export-dired)
               embark-exporters-alist)))
    (embark-export)))

;;;###autoload
(defun +consult-grep--export-dired (candidates)
  "Create a Dired buffer for files from Consult grep CANDIDATES."
  (let ((files (delete-dups
                (delq nil
                      (mapcar #'+consult-grep--candidate-file candidates)))))
    (if files
        (+grep-embark-export-dired-unique files)
      (user-error "No files in grep candidates"))))

;;;###autoload
(defun +consult-grep--candidate-file (candidate)
  "Return the file path from a Consult grep CANDIDATE."
  (when (stringp candidate)
    (when-let ((file (or (get-text-property 0 'consult--prefix-group candidate)
                         (when-let ((file-end (next-single-property-change
                                               0 'face candidate)))
                           (substring-no-properties candidate 0 file-end)))))
      (expand-file-name file))))

;;;###autoload
(defun +consult-find-todos ()
  "Search all todos."
  (interactive)
  (consult-line "TODO"))

;;;###autoload
(defun +consult-org-agenda-todos (&optional match)
  "Jump to an Org agenda TODO heading.

MATCH is as in `org-map-entries'."
  (interactive)
  (require 'consult-org)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (let ((prefix t))
    (consult--read
     (consult--slow-operation "Collecting headings..."
       (or (consult-org--headings prefix match 'agenda)
           (user-error "No headings")))
     :prompt "Go to heading: "
     :category 'org-heading
     :sort nil
     :require-match t
     :history '(:input consult-org--history)
     :narrow (consult-org--narrow)
     :initial-narrow ?t
     :state (consult--jump-state)
     :annotate #'consult-org--annotate
     :group #'consult-org--group
     :lookup (apply-partially #'consult--lookup-prop 'org-marker))))

;;;###autoload
(defun +consult-find-bound-function (key-sequence)
  "Goto the definition of the command bound to `KEY-SEQUENCE'"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  key-sequence))
     ((commandp sym t)
      (consult-ripgrep "~/.emacs.d" (format "defun %s" (symbol-name sym))))
     (t
      (user-error "%s is bound to %s which is not a command"
                  (key-description key-sequence)
                  sym)))))

;;;###autoload
(defun +consult-find-key-binding (key-sequence)
  "Goto the key binding form for the command bound to KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence))
        (last-key (substring (key-description key-sequence) -1)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  key-sequence))
     ((commandp sym t)
      (consult-ripgrep "~/.emacs.d"
                       (format "%s\" \\..*%s)"
                               (regexp-quote last-key)
                               (regexp-quote (symbol-name sym)))))
     (t
      (user-error "%s is bound to %s which is not a command"
                  (key-description key-sequence)
                  sym)))))

;;;###autoload
(defun +consult-kmacro ()
  (interactive)
  (with-undo-amalgamate
    (call-interactively #'consult-kmacro)))

(defvar-local consult-toggle-preview-orig nil)

;;;###autoload
(defun +consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))

;;;###autoload
(defun +consult-find-package ()
  (interactive)
  (consult-ripgrep "~/.emacs.d/" "use-package "))

;;;###autoload
(defun +consult-project-find-todos ()
  (interactive)
  (consult-ripgrep (project-root (project-current t)) "TODO"))

;;;###autoload
(defun +consult-narrow-help ()
  "Show prefix help for `consult-narrow-key'."
  (interactive)
  (consult--require-minibuffer)
  (unless consult-narrow-key
    (user-error "`consult-narrow-key' is not configured"))
  (setq unread-command-events
        (append (listify-key-sequence
                 (vconcat (consult--key-parse consult-narrow-key)
                          (kbd "C-h")))
                unread-command-events)))

(provide 'consult-extras)
