;;; consult-project-tab.el --- Consult commands for project tabs -*- lexical-binding: t -*-

(require 'consult)
(require 'project-tab)
(require 'seq)
(require 'tab-bar)

;;;###autoload
(defun +consult-tab ()
  "Select tab with completion and preview."
  (interactive)
  (+consult-tab--select-action
   (consult--read (+consult-tab--candidates)
                  :prompt (format-prompt "Switch to tab by name"
                                         (car (+consult-tab--recent-tab-names)))
                  :category 'tab
                  :history '+consult-tab--history
                  :sort nil
                  :lookup #'consult--lookup-candidate
                  :state (+consult-tab--preview))))

(defvar +consult-tab--history nil
  "History of tab completion selections.")

;;;###autoload
(defun +consult-tab--candidates ()
  "Return tab completion candidates with tab indexes as lookup values."
  (mapcar (lambda (tab)
            (let* ((index (1+ (tab-bar--tab-index tab)))
                   (name (alist-get 'name tab)))
              (propertize name 'consult--candidate index)))
          (tab-bar--tabs-recent)))

;;;###autoload
(defun +consult-tab--recent-tab-names ()
  "Return recent tab names for prompt defaults."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

;;;###autoload
(defun +consult-tab--tab-from-index (index)
  "Return tab at one-based INDEX."
  (when index
    (nth (1- index) (funcall tab-bar-tabs-function))))

;;;###autoload
(defun +consult-tab--preview ()
  "Return preview function for tabs."
  (let ((orig-wc (current-window-configuration)))
    (lambda (action cand)
      (cond
       ((or (eq action 'exit)
            (and (eq action 'preview) (null cand)))
        (set-window-configuration orig-wc nil t))
       ((and (eq action 'preview) cand)
        (let ((tab (+consult-tab--tab-from-index cand)))
          (when tab
            (if (eq (car tab) 'current-tab)
                (set-window-configuration orig-wc nil t)
              (+consult-tab--preview-tab tab)))))))))

;;;###autoload
(defun +consult-tab--preview-tab (tab)
  "Preview the window configuration saved in TAB."
  (when-let* ((state (alist-get 'ws tab)))
    (delete-other-windows)
    (window-state-put (copy-tree state) (selected-window) 'safe)))

;;;###autoload
(defun +consult-tab--select-action (index)
  "Select tab at one-based INDEX."
  (tab-bar-select-tab index))

;;;###autoload
(defun +consult-project-tab-find ()
  "Select a tab in the current project with completion.
With no current project, fall back to `+consult-tab'."
  (interactive)
  (if (+project-tab--current-prefix)
      (if-let ((tabs (+consult-project-tab--tabs)))
          (tab-bar-select-tab
           (consult--read (+consult-project-tab--candidates tabs)
                          :prompt "Switch to project tab: "
                          :category 'tab
                          :history '+consult-project-tab--history
                          :sort nil
                          :lookup #'consult--lookup-candidate
                          :state (+consult-project-tab--preview)))
        (user-error "No tabs for current project"))
    (+consult-tab)))

(define-key +project-tab-repeat-map (kbd "f") #'+consult-project-tab-find)

(defvar +consult-project-tab--history nil
  "History of project tab completion selections.")

;;;###autoload
(defun +consult-project-tab--tabs ()
  "Return recent tabs whose names belong to the current project."
  (when-let ((prefix (+project-tab--current-prefix)))
    (seq-filter (lambda (tab)
                  (+project-tab--tab-p tab prefix))
                (tab-bar--tabs-recent))))

;;;###autoload
(defun +consult-project-tab--candidates (tabs)
  "Return completion candidates for TABS with tab indexes as lookup values."
  (mapcar (lambda (tab)
            (let* ((index (1+ (tab-bar--tab-index tab)))
                   (name (alist-get 'name tab)))
              (propertize name 'consult--candidate index)))
          tabs))

;;;###autoload
(defun +consult-project-tab--preview ()
  "Return preview function for tabs."
  (let ((orig-wc (current-window-configuration)))
    (lambda (action cand)
      (cond
       ((or (eq action 'exit)
            (and (eq action 'preview) (null cand)))
        (set-window-configuration orig-wc nil t))
       ((and (eq action 'preview) cand)
        (let ((tab (+consult-project-tab--tab-from-index cand)))
          (when tab
            (if (+project-tab--current-tab-p tab)
                (set-window-configuration orig-wc nil t)
              (+consult-project-tab--preview-tab tab)))))))))

;;;###autoload
(defun +consult-project-tab--tab-from-index (index)
  "Return tab at one-based INDEX."
  (when index
    (nth (1- index) (funcall tab-bar-tabs-function))))

;;;###autoload
(defun +consult-project-tab--preview-tab (tab)
  "Preview the window configuration saved in TAB."
  (when-let ((state (alist-get 'ws tab)))
    (delete-other-windows)
    (window-state-put (copy-tree state) (selected-window) 'safe)))

(defvar embark-become-keymaps)

(defvar-keymap +consult-project-tab-become-map
  :doc "Embark become map for project tab commands."
  "F" #'+consult-tab
  "f" #'+consult-project-tab-find
  "n" #'+project-tab-new-project-command)

(with-eval-after-load 'embark
  (add-to-list 'embark-become-keymaps '+consult-project-tab-become-map))

(provide 'consult-project-tab)
