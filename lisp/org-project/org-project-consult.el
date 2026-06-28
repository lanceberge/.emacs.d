;;; org-project-consult.el --- Consult integration for Org projects -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'consult)
(require 'consult-org)
(require 'embark)
(require 'embark-org)
(require 'org)
(require 'org-project)

(defgroup +org-project-consult nil
  "Consult integration for Org projects."
  :group '+org-project)

(defcustom +org-project-consult-preview-files t
  "Whether Org project file prompts preview candidates."
  :type 'boolean
  :group '+org-project-consult)

(defvar +org-project-consult--embark-target nil
  "The Org project Consult target currently being acted on by Embark.")

(defvar +org-project-consult--agenda-minibuffer nil
  "Non-nil in an Org project Consult agenda minibuffer.")

;;;###autoload
(defun +org-project-consult-agenda (&optional match project-file)
  "Prompt for an Org project file, then jump to one of its headings.

MATCH is as in `org-map-entries'."
  (interactive)
  (let ((org-agenda-files (list (or project-file
                                    (funcall +org-project-prompt-function)))))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local +org-project-consult--agenda-minibuffer t))
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
         :lookup (apply-partially #'consult--lookup-prop 'org-marker))))))

;;;###autoload
(defun +org-project-consult-file ()
  "Prompt for and visit an Org project file."
  (interactive)
  (find-file (funcall +org-project-prompt-function)))

;;;###autoload
(defun +org-project-consult-agenda-for-current-project (&optional match)
  "Jump to a heading in the current project's remembered Org project file.

MATCH is as in `org-map-entries'."
  (interactive)
  (+org-project-consult-agenda
   match
   (funcall +org-project-obtain-project-function)))

;;;###autoload
(defun +org-project-consult-file-for-current-project ()
  "Visit the Org project file obtained for the current directory."
  (interactive)
  (find-file (funcall +org-project-obtain-project-function)))

;;;###autoload
(define-minor-mode +org-project-consult-mode
  "Use Consult when prompting for Org project files."
  :global t
  (if +org-project-consult-mode
      (setq +org-project-prompt-function #'+org-project-consult--prompt-file)
    (setq +org-project-prompt-function #'+org-project--prompt-file)))

;;;###autoload
(defun +org-project-consult-add-todo ()
  "Add a TODO to the selected Org project file."
  (interactive)
  (let ((project-file (+org-project-consult--embark-file-target)))
    (+org-project-add-todo project-file)))

;;;###autoload
(defun +org-project-consult-add-todo-for-current-project ()
  "Remember the selected Org project file, then add a TODO to it."
  (interactive)
  (let ((project-file (+org-project-consult--embark-file-target)))
    (+org-project--remember-project-file project-file)
    (+org-project-add-todo project-file)))

;;;###autoload
(defun +org-project-consult-mark-done ()
  "Mark the selected Org project heading as done."
  (interactive)
  (+org-project-mark-done (+org-project-consult--embark-marker-target)))

;;;###autoload
(defun +org-project-consult-mark-heading-done ()
  "Mark the Org heading targeted by Embark as done and file it under Done."
  (interactive)
  (+org-project-mark-done (copy-marker (line-beginning-position) t)))

;;;###autoload
(cl-defun +org-project-consult--at-marked-heading (&rest args
                                                        &key run target
                                                        &allow-other-keys)
  "Run Embark action at TARGET when TARGET carries an Org marker."
  (if (and target (get-text-property 0 'org-marker target))
      (apply #'embark-org--at-heading args)
    (apply run args)))

;;;###autoload
(defun +org-project-consult-delete-todo ()
  "Delete the selected Org project heading."
  (interactive)
  (+org-project-delete-heading (+org-project-consult--embark-marker-target)))

(defvar-keymap +org-project-consult-file-map
  :doc "Embark actions for Org project files."
  "t" #'+org-project-consult-add-todo
  "T" #'+org-project-consult-add-todo-for-current-project)

(defvar-keymap +org-project-consult-heading-map
  :doc "Embark actions for Org project headings."
  "d" #'+org-project-consult-delete-todo
  "x" #'+org-project-consult-mark-done)

(keymap-set embark-org-heading-map "x" #'+org-project-consult-mark-heading-done)

(setf (alist-get 'org-heading embark-default-action-overrides)
      #'+org-project-consult-mark-heading-done)

;;;###autoload
(defun +org-project-consult--prompt-file ()
  "Prompt for an Org project file using Consult."
  (consult--read
   (+org-project-consult--file-candidates)
   :prompt "Select project file: "
   :category 'org-project-file
   :require-match nil
   :sort nil
   :lookup #'+org-project-consult--lookup-file
   :state (when +org-project-consult-preview-files
            (consult--file-preview))
   :history 'file-name-history))

;;;###autoload
(defun +org-project-consult--file-candidates ()
  "Return Org project file candidates."
  (directory-files +org-project-directory t "\\.org$"))

;;;###autoload
(defun +org-project-consult--lookup-file (selected candidates input _narrow)
  "Return SELECTED Org project file or resolve INPUT as a new project file."
  (if (member selected candidates)
      selected
    (expand-file-name (or selected input) +org-project-directory)))

;;;###autoload
(defun +org-project-consult--store-embark-target (&rest args)
  "Store the Embark target from ARGS for Org project actions."
  (setq +org-project-consult--embark-target args))

;;;###autoload
(defun +org-project-consult--embark-file-target ()
  "Return the file selected by the current Org project Embark action."
  (or (plist-get +org-project-consult--embark-target :target)
      (user-error "No Org project file target")))

;;;###autoload
(defun +org-project-consult--embark-marker-target ()
  "Return the Org marker selected by the current Org project Embark action."
  (let* ((target (plist-get +org-project-consult--embark-target :target))
         (marker (and target (get-text-property 0 'org-marker target))))
    (or marker
        (user-error "No Org project heading target"))))

;;;###autoload
(defun +org-project-consult--add-heading-actions (keymap)
  "Add Org project Consult heading actions to KEYMAP when appropriate."
  (if (and (minibufferp)
           (bound-and-true-p +org-project-consult--agenda-minibuffer))
      (make-composed-keymap +org-project-consult-heading-map keymap)
    keymap))

(setq embark-keymap-alist
      (assq-delete-all 'org-project-file embark-keymap-alist))

(add-to-list 'embark-keymap-alist
             '(org-project-file . +org-project-consult-file-map))

(dolist (action '(+org-project-consult-add-todo
                  +org-project-consult-add-todo-for-current-project
                  +org-project-consult-mark-done
                  +org-project-consult-delete-todo))
  (setf (alist-get action embark-pre-action-hooks)
        (list #'+org-project-consult--store-embark-target)))

(cl-pushnew '+org-project-consult--at-marked-heading
            (alist-get '+org-project-consult-mark-heading-done
                       embark-around-action-hooks))

(advice-remove #'embark--action-keymap
               #'+org-project-consult--add-heading-actions)
(advice-add #'embark--action-keymap
            :filter-return #'+org-project-consult--add-heading-actions)

(provide 'org-project-consult)
;;; org-project-consult.el ends here
