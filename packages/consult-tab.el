;;; -*- lexical-binding: t -*-
;; from https://github.com/minad/consult/wiki

(require 'consult)
(require 'embark)
(require 'marginalia)

(defvar +consult--tab-index-current-tab-name nil
  "The name of the current tab. Needed for marginalia annotations when previewing tabs.
Because we are changing the current window configuration when previewing tabs, we are
also changing the name of the current tab unless it's not an explicit name. To prevent
this, we can store the name of the current tab before calling consult command and use
this saved name in marginalia annotations of the current tab.")

(defvar +consult--tab-index-current-tab-bufs nil
  "List of current tab buffer names. Needed for marginalia annotations when previewing tabs.
Because we are changing the current window configuration when previewing tabs, we need to
save the current list of buffers displayed in windows before calling consult command and
use this saved list in marginalia annotations of the current tab.")

(defun +marginalia-annotate-tab-index (cand)
  "Modified version of `marginalia-annotate-tab' suited for tab-index completion."
  (let* ((tab (nth (1- (string-to-number cand)) (tab-bar-tabs)))
         (current-p (memq 'current-tab tab))
         (ws (alist-get 'ws tab))
         (bufs (if current-p
                   +consult--tab-index-current-tab-bufs
                 (window-state-buffers ws))))
    ;; NOTE: When the buffer key is present in the window state
    ;; it is added in front of the window buffer list and gets duplicated.
    (unless current-p
      (when (cadr (assq 'buffer ws)) (pop bufs)))
    (marginalia--fields
     ;; Tab name
     ((if current-p
          +consult--tab-index-current-tab-name
        (alist-get 'name tab))
      :face (if current-p 'marginalia-on 'marginalia-key)
      :width 15
      :truncate 15)
     ;; Window count
     ((if (cdr bufs)
          (format "%d windows" (length bufs))
        "1 window ")
      :face 'marginalia-size
      :width 15)
     ;; List of buffers
     ((string-join bufs " \t ")
      :face 'marginalia-documentation))))

(add-to-list 'marginalia-annotators '(tab-index +marginalia-annotate-tab-index))

(defvar-keymap +embark-tab-index-map
  :doc "Keymap for actions on tab indexes."
  "RET" #'+tab-bar-select-tab-by-index
  "s" #'+tab-bar-select-tab-by-index
  "k" #'+tab-bar-close-tab-by-index
  "r" #'+tab-bar-rename-tab-by-index)

(add-to-list 'embark-keymap-alist '(tab-index +embark-tab-index-map))

;;;###autoload
(defun +tab-bar-select-tab-by-index (index)
  "Select tab by INDEX string."
  (interactive "sSelect tab by index: ")
  (tab-bar-select-tab (string-to-number index)))

;;;###autoload
(defun +tab-bar-close-tab-by-index (index)
  "Close tab by INDEX string."
  (interactive "sClose tab by index: ")
  (tab-bar-close-tab (string-to-number index)))

;;;###autoload
(defun +tab-bar-rename-tab-by-index (index new-name)
  "Rename tab by INDEX string to NEW-NAME."
  (interactive
   (let* ((index (read-string "Rename tab by index: "))
          (tab (nth (1- (string-to-number index)) (tab-bar-tabs)))
          (name (alist-get 'name tab)))
     (list index
           (read-from-minibuffer
            "New name for tab (leave blank for automatic naming): "
            nil nil nil nil name))))
  (tab-bar-rename-tab new-name (string-to-number index)))

(defun +consult--tab-index-preview ()
  "Preview function for tab-index."
  (let ((orig-index (1+ (tab-bar--current-tab-index))))
    (lambda (action cand)
      (if (or (and (eq action 'preview) (null cand))
              (memq action '(exit return)))
          (tab-bar-select-tab orig-index)
        (when cand
          (tab-bar-select-tab (string-to-number cand)))))))

(defvar +consult--source-tab-index
  (list :name "Tab"
        :category 'tab-index
        :default t
        :narrow ?t
        :state #'+consult--tab-index-preview
        :items (lambda ()
                 (mapcar #'number-to-string
                         (number-sequence 1 (length (tab-bar-tabs))))))
  "Source of all tab indexes starting from 1.")

(defun +consult--tab-index (&optional prompt)
  "Prompt for tab selection and return selected candidate as number.
Replace prompt with PROMPT if specified."
  ;; Marginalia integration
  (let (;; Align annotations as close to index as possible
        (marginalia-align-offset -18)
        ;; Save curret tab name
        (+consult--tab-index-current-tab-name (alist-get 'name (tab-bar--current-tab)))
        ;; Save current window buffer list
        (+consult--tab-index-current-tab-bufs (mapcar #'buffer-name
                                                      (mapcar #'window-buffer
                                                              (window-list)))))
    (string-to-number (consult--read (funcall (plist-get +consult--source-tab-index :items))
                                     :category 'tab-index
                                     :state (+consult--tab-index-preview)
                                     ;; disable sorting
                                     :sort nil
                                     :require-match t
                                     :prompt (or prompt "Select tab: ")))))

;;;###autoload
(defun +consult-tab ()
  "Select tab and switch to it."
  (interactive)
  (tab-bar-select-tab (+consult--tab-index)))
