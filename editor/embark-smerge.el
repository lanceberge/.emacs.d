;;; embark-smerge.el --- Embark actions for smerge conflicts -*- lexical-binding: t; -*-

(use-package +embark-smerge
  :ensure nil
  :after (smerge embark)
  :config
  (add-to-list 'embark-keymap-alist '(conflict . +embark-smerge-conflict-map))
  (add-to-list 'embark-target-finders '+embark-smerge-target-conflict-at-point)

  (defvar-keymap +embark-smerge-conflict-map
    :doc "Keymap for actions related to Merge Conflicts"
    :parent embark-general-map
    "a" #'smerge-keep-all
    "b" #'smerge-keep-base
    "c" #'smerge-combine-with-next
    "d" #'smerge-ediff
    "l" #'smerge-keep-lower
    "n" #'smerge-next
    "p" #'smerge-prev
    "r" #'smerge-resolve
    "R" #'smerge-refine
    "u" #'smerge-keep-upper))

;;;###autoload
(defun +embark-smerge-target-conflict-at-point ()
  "Target a Merge Conflict at point."
  (when smerge-mode
    (when-let* ((beg (save-excursion (when (search-backward "<<<<<<<" nil t)
                                       (move-to-left-margin)
                                       (point))))
                (end (save-excursion (when (search-forward ">>>>>>>" nil t)
                                       (move-end-of-line nil)
                                       (point))))
                (str (buffer-substring-no-properties beg end)))
      (save-match-data
        (when (string-match "<<<<<<<.*?=======.*?>>>>>>>" (string-replace "\n" "" str))
          `(conflict "test" ,beg . ,end))))))
