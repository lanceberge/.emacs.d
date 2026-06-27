;;; -*- lexical-binding: t -*-
;; from https://github.com/minad/consult/wiki

(require 'consult)
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

;;;###autoload
(defun +consult-tab-close ()
  "Select tab to close it."
  (interactive)
  (tab-bar-close-tab
   (consult--read (+consult-tab--candidates)
                  :prompt "Close tab by name: "
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

(provide 'consult-tab)
