;;; -*- lexical-binding: t -*-
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :bind
  (:map +leader-map
        ("to" . #'+tab-bar/open-and-rename)
        ("tl" . #'tab-bar-switch-to-recent-tab)
        ("t SPC g" . #'tab-bar-select-tab)
        ("tg" . #'tab-bar-switch-to-tab)
        ("tn" . #'tab-bar-switch-to-next-tab)
        ("tp" . #'tab-bar-switch-to-prev-tab)
        ("t SPC d" . #'tab-bar-close-tab-by-name)
        ("t,d" . #'tab-bar-close-other-tabs)
        ("td" . #'tab-bar-close-tab)
        ("tu" . #'tab-bar-undo-close-tab)
        ("t SPC r" . #'tab-bar-rename-tab-by-name)
        ("po" . #'+open-project)
        ("tr" . #'tab-bar-rename-tab)
        ("wu" . #'tab-bar-history-back)
        ("wr" . #'tab-bar-history-forward)))

;;;###autoload
(defun +tab-bar/open-and-rename ()
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'tab-bar-rename-tab))

;;;###autoload
(defun +open-tab-if-exists (tab-name)
  (let ((tabs (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
    (if (member tab-name tabs)
        (tab-bar-switch-to-tab tab-name)
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab tab-name)))))
