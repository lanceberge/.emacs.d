;;; -*- lexical-binding: t -*-
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :bind
  (:map ctl-x-map
        ("1" . #'+window-toggle-tab-zoom)))

(use-package project-tab
  :ensure (:type file :main "~/.emacs.d/lisp/project-tab.el" :files ("project-tab.el"))
  :custom
  (tab-bar-tab-name-function #'+project-tab-name-tab-function)
  :bind
  (:map project-prefix-map
        ("[" . #'+project-tab-prev-project-command)
        ("]" . #'+project-tab-next-project-command)
        ("n" . #'+project-tab-new-project-command)
        ("j" . #'+project-tab-switch-other-project-command)
        ("p" . #'+project-tab-switch-project-command))
  (:map ctl-x-map
        ("tn" . #'+project-tab-new-project-command))
  (:map +leader-map
        ("k" . #'+project-tab-switch-project-command)
        ("j" . #'+project-tab-switch-other-project-command))
  (:map +forward-map
        ("t" . #'+project-tab-next))
  (:map +backward-map
        ("t" . #'+project-tab-prev)))

;;;###autoload
(defun +project-reload-and-switch ()
  (interactive)
  (+project-load-projects)
  (call-interactively #'+project-switch-project))

(use-package consult-project-tab
  :ensure (:type file :main "~/.emacs.d/lisp/consult-project-tab.el" :files ("consult-project-tab.el"))
  :bind
  (:map ctl-x-map
        ("tF" . #'+consult-tab)
        ("tf" . #'+consult-project-tab-find)))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  ;; (plist-put consult-source-buffer :hidden t)
  ;; (plist-put consult-source-buffer :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))
