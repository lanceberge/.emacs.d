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
        ("F" . #'+kubernetes-port-forward-start)
        ("K" . #'+kubernetes-port-forward-kill)
        ("X" . #'kubernetes-contexts-use-context))
  (:map +modal-kubernetes-motion-mode-map
        ("x" . #'kubernetes-execute-marks)))

;; Port-forward stuff is from: https://github.com/kubernetes-el/kubernetes-el/commit/01305e7

(defvar +kubernetes-port-forward-buffer-prefix "*kubernetes port-forward: "
  "Prefix for Kubernetes port-forward buffers.")

;;;###autoload
(defun +kubernetes-port-forward-start (resource local-port target-port)
  "Forward LOCAL-PORT to TARGET-PORT on RESOURCE, a pod or deployment.
Interactively, use the resource at point or prompt for one."
  (interactive
   (let* ((state (kubernetes-state))
          (resource (or (kubernetes-utils-get-resource-info-at-point)
                        (let ((type (completing-read "Resource type: "
                                                     '("pod" "deployment") nil t)))
                          (cons type (kubernetes-utils-get-resource-name state type)))))
          (local-port (read-number "Local port: "))
          (target-port (read-number "Target port: ")))
     (list resource local-port target-port)))
  (let* ((state (kubernetes-state))
         (type (if (consp resource) (car resource) "pod"))
         (name (if (consp resource) (cdr resource) resource))
         (pod-name (+kubernetes-port-forward--resolve-pod state type name))
         (namespace (kubernetes-state--get state 'current-namespace))
         (port-spec (format "%s:%s" local-port target-port))
         (buffer-name (format "%s%s%s %s*"
                              +kubernetes-port-forward-buffer-prefix
                              (if namespace (concat namespace "/") "")
                              pod-name port-spec))
         (args (append (list "port-forward" (format "pod/%s" pod-name) port-spec)
                       (kubernetes-kubectl--flags-from-state state))))
    (when (get-buffer-process buffer-name)
      (user-error "Port-forward already running in %s" buffer-name))
    (kubernetes-utils-process-buffer-start
     buffer-name #'kubernetes-mode kubernetes-kubectl-executable args)
    (message "Started port-forward to %s (local %s -> target %s)"
             pod-name local-port target-port)))

;;;###autoload
(defun +kubernetes-port-forward-kill ()
  "Select a Kubernetes port-forward buffer and stop its process."
  (interactive)
  (let ((names (seq-filter
                (lambda (name)
                  (string-prefix-p +kubernetes-port-forward-buffer-prefix name))
                (mapcar #'buffer-name (buffer-list)))))
    (if (null names)
        (message "No Kubernetes port-forward buffers found")
      (let* ((name (completing-read "Kill port-forward: " names nil t))
             (buffer (get-buffer name))
             (process (and buffer (get-buffer-process buffer))))
        (if process
            (kubernetes-process-kill-quietly process)
          (kill-buffer buffer))
        (message "Killed %s" name)))))

;;;###autoload
(defun +kubernetes-port-forward--resolve-pod (state type name)
  "Resolve a pod NAME from resource TYPE using STATE."
  (pcase type
    ("pod" name)
    ("deployment"
     (let ((names (mapcar (lambda (pod)
                            (alist-get 'name (alist-get 'metadata pod)))
                          (kubernetes-utils--get-all-pods-for-owner
                           "deployment" name state))))
       (pcase names
         ('() (user-error "No pods found for deployment %s" name))
         (`(,pod) pod)
         (_ (completing-read (format "Pod for deployment %s: " name)
                             names nil t)))))
    (_ (user-error "Port-forward supports pod or deployment; found: %s" type))))

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
