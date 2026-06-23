;;; -*- lexical-binding: t -*-

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
(defun +consult-project-file-here ()
  "Find a project file under `default-directory'."
  (interactive)
  (+consult--project-file-at-dir default-directory))

;;;###autoload
(defun +consult-preview-tramp-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from consult preview.
Excludes Tramp buffers so preview never opens a remote connection."
  (when-let ((dir (buffer-local-value 'default-directory buffer)))
    (file-remote-p dir)))

;;;###autoload
(defun +consult--buffer-in-dir (dir)
  (let ((project (project-current nil dir)))
    (let ((default-directory (project-root project)))
      (consult--with-project
        (consult-buffer consult-project-buffer-sources)))))

;;;###autoload
(defun +consult-focus-lines ()
  (interactive)
  (forward-char 1)
  (call-interactively #'consult-focus-lines))

;;;###autoload
(defun +consult-keep-lines ()
  (interactive)
  (forward-char 1)
  (call-interactively #'consult-keep-lines))

;;;###autoload
(defun +consult-yank-or-replace ()
  "If region is active, replace it with selected text from kill ring using consult-yank-pop.
Otherwise, just call consult-yank-pop."
  (interactive)
  (if (region-active-p)
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (defun +consult-yank-replace-region (&rest _)
          (when (region-active-p)
            (delete-region region-start region-end)))
        (unwind-protect
            (progn
              (add-function :after (symbol-function 'consult-yank-pop) #'+consult-yank-replace-region)
              (call-interactively #'consult-yank-pop))
          (remove-function (symbol-function 'consult-yank-pop) #'+consult-yank-replace-region)))
    (call-interactively #'consult-yank-pop)))

;;;###autoload
(defun +consult-unfocus-lines ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively #'consult-focus-lines)))

;;;###autoload
(defun +consult-line ()
  (interactive)
  (call-interactively #'consult-line)
  (let ((consult-search (car consult--line-history)))
    (unless (string-match-p " " consult-search)
      (+isearch-update-last-search consult-search))))

;;;###autoload
(defun +consult-line-multi ()
  (interactive)
  (call-interactively #'consult-line-multi)
  (let ((consult-search (car consult--line-multi-history)))
    (unless (string-match-p " " consult-search)
      (+isearch-update-last-search consult-search))))

;;;###autoload
(defun +consult-ripgrep-here ()
  (interactive)
  (consult-ripgrep default-directory nil))

;;;###autoload
(defun +consult--buffer (&optional sources initial-narrow initial)
  "Run `consult-buffer' with SOURCES, INITIAL-NARROW, and INITIAL."
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Switch to: "
                                  :history 'consult--buffer-history
                                  :sort nil
                                  :initial initial
                                  :initial-narrow initial-narrow)))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

;;;###autoload
(defun +consult-project-buffer (&optional initial)
  (interactive)
  (if (project-current nil)
      (consult--with-project
        (+consult--buffer consult-project-buffer-sources ?b initial))
    (+consult--buffer consult-buffer-sources ?b initial)))

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
(defun +consult-find-key (key-sequence)
  "Goto the definition of the command bound to `KEY-SEQUENCE'"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  key-sequence))
     ((commandp sym t)
      (consult-ripgrep "~/.emacs.d" (symbol-name sym)))
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

(provide 'consult-extensions)
