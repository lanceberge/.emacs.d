;;; -*- lexical-binding: t -*-
(define-prefix-command '+kubernetes-prefix-map)

(use-package kubernetes
  :init
  (+kubed-setup-contexts)
  (+modal-define-intersection-mode kubernetes-mode +motion-mode)
  :custom
  (kubernetes-default-overview-view 'pods)
  (add-hook 'kubernetes-redraw-hook #'+kubenetes-apply-section-visibility 90)
  :bind
  (:map +leader-map
        ("k" . nil)
        ("k" . #'+kubernetes-prefix-map))
  (:map +kubernetes-prefix-map
        ("x" . #'kubernetes-contexts-use-context)
        ("n" . #'kubernetes-set-namespace)
        ("p" . #'kubernetes-overview))
  (:map kubernetes-mode-map
        ("N" . #'kubernetes-set-namespace)
        ("X" . #'kubernetes-contexts-use-context))
  (:map +modal-kubernetes-motion-mode-map
        ("x" . #'kubernetes-execute-marks)))

(use-package kubed
  :disabled t
  :init
  (+kubed-setup-contexts)
  (+modal-define-intersection-mode kubed-pods-mode +motion-mode)
  :bind
  (:map +leader-map
        ("k" . nil)
        ("k" . kubed-prefix-map))
  (:map kubed-prefix-map
        ("x" . #'+kubed-use-context)
        ("n" . #'kubed-set-namespace)
        ("p" . #'kubed-list-pods))
  (:map +modal-kubed-pods-motion-mode-map
        ("x" . #'kubed-list-delete-marked)))

;;;###autoload
(defun +kubed-setup-contexts ()
  (let ((kube-dir (expand-file-name "~/.kube")))
    (when (file-directory-p kube-dir)
      (let ((configs (seq-filter
                      (lambda (file)
                        (and (file-regular-p file)
                             (not (file-symlink-p file))))
                      (directory-files kube-dir t))))
        (when configs
          (setenv "KUBECONFIG" (mapconcat #'identity configs path-separator))))))())

;;;###autoload
(defun +kubed-use-context ()
  (interactive)
  (+kubed-setup-contexts)
  (call-interactiely #'kubed-use-context))
