;;; project-tab.el --- Project-scoped tab commands -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'project)
(require 'repeat)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'project-extras)

(defgroup +project-tab nil
  "Project-scoped tab commands."
  :group 'convenience)

(defcustom +project-tab-next-create nil
  "How `+project-tab-next' behaves when there is no next project tab.
Nil means signal an error.  `prompt' means ask before creating a tab.
`auto' means create a tab automatically."
  :type '(choice (const :tag "Do not create" nil)
                 (const :tag "Prompt" prompt)
                 (const :tag "Auto" auto))
  :group '+project-tab)

(defcustom +project-tab-prev-create nil
  "How `+project-tab-prev' behaves when there is no previous project tab.
Nil means signal an error.  `prompt' means ask before creating a tab.
`auto' means create a tab automatically."
  :type '(choice (const :tag "Do not create" nil)
                 (const :tag "Prompt" prompt)
                 (const :tag "Auto" auto))
  :group '+project-tab)

(defcustom +project-tab-other-project-missing-command nil
  "Command called when there is no other tab in the current project.
Nil means signal an error."
  :type '(choice (const :tag "Signal an error" nil)
                 (function :tag "Command"))
  :group '+project-tab)

(defcustom +tab-bar-next-create-command #'tab-new
  "Command called by `+project-tab-next' when creating a tab."
  :type 'function
  :group '+project-tab)

(defcustom +tab-bar-prev-create-command #'tab-new
  "Command called by `+project-tab-prev' when creating a tab."
  :type 'function
  :group '+project-tab)

(defvar-keymap +project-tab-repeat-map
  :doc "Repeat map for project tab commands."
  :repeat (:exit (+project-tab-new-project-command))
  "]" #'+project-tab-next
  "[" #'+project-tab-prev
  "0" #'tab-bar-close-tab
  "o" #'+project-tab-other-project
  "p" #'project-prefix-or-any-command
  "2" #'+project-tab-new-project-command)

;;;###autoload
(defun +project-tab-new (&optional arg)
  "Create a new tab using `tab-bar-new-tab'."
  (interactive "P")
  (let ((root (+project-tab--current-project-root)))
    (tab-bar-new-tab arg)
    (when root
      (+project-tab--pin-current-project root))))

;;;###autoload
(defun +project-tab-new-project-command (&optional dir)
  "Run a project command for DIR, displaying the resulting buffer in a new tab.
DIR defaults to the current project root.
The following commands are available:
\\{project-prefix-map}"
  (interactive)
  (let ((root (+project-tab--project-root
               (or dir
                   (+project-tab--current-project-root)
                   (project-root (project-current t))))))
    (tab-bar-new-tab)
    (+project-tab--pin-current-project root)
    (+project-call-project-command root)))

;;;###autoload
(defun +project-tab-switch-project-command (dir)
  "Switch to DIR's most recent project tab, then read and run a project command.
With a prefix argument, call `+project-load-projects' before prompting."
  (interactive
   (progn
     (when current-prefix-arg
       (+project-load-projects))
     (list (funcall project-prompter))))
  (+project-tab--switch-to-project-and-command dir))

;;;###autoload
(defun +project-tab-switch-other-project-command (dir)
  "Switch to the other project's most recent tab, then read and run a command."
  (interactive
   (list (+project-last-opened-other-project-root
          (+project-tab--current-root))))
  (+project-tab--switch-to-project-and-command dir))

;;;###autoload
(defun +project-tab-other-project-command ()
  "Switch to the most recently used other project tab and read a project command."
  (interactive)
  (when (+project-tab--select-other)
    (+project-call-project-command (+project-tab--current-root))))

;;;###autoload
(defun +project-tab-next-project-command (&optional arg)
  "Switch to the next project tab, then read a project command."
  (interactive "p")
  (+project-tab-next arg)
  (+project-call-project-command (+project-tab--current-root)))

;;;###autoload
(defun +project-tab-prev-project-command (&optional arg)
  "Switch to the previous project tab, then read a project command."
  (interactive "p")
  (+project-tab-prev arg)
  (+project-call-project-command (+project-tab--current-root)))

;;;###autoload
(defun +project-tab-next (&optional arg)
  "Switch to the next tab in the current project.
With no current project, fall back to `tab-bar-switch-to-next-tab'."
  (interactive "p")
  (+project-tab--switch arg +project-tab-next-create +tab-bar-next-create-command))

;;;###autoload
(defun +project-tab-prev (&optional arg)
  "Switch to the previous tab in the current project.
With no current project, fall back to `tab-bar-switch-to-prev-tab'."
  (interactive "p")
  (+project-tab--switch (- arg) +project-tab-prev-create +tab-bar-prev-create-command))

;;;###autoload
(defun +project-tab-other-project ()
  "Switch to the most recently used other tab in the current project.
With no current project, fall back to `tab-bar-switch-to-recent-tab'."
  (interactive)
  (+project-tab--select-other))

;;;###autoload
(defun +project-tab-reload-and-switch-project ()
  (interactive)
  (+project-load-projects)
  (call-interactively #'+project-tab-switch-project-command))

;;;###autoload
(defun +project-tab-switch-to-project (dir &optional create)
  "Switch to the most recently used tab for project DIR.
When CREATE is non-nil, create a new tab if no existing project tab is found."
  (let ((root (+project-tab--project-root dir)))
    (if-let ((tab (seq-find (lambda (tab)
                              (+project-tab--tab-p tab root))
                            (tab-bar--tabs-recent))))
        (tab-bar-select-tab (1+ (tab-bar--tab-index tab)))
      (when create
        (tab-bar-new-tab)
        (+project-tab--pin-current-project root)))))

;;;###autoload
(defun +project-tab-project-prefix (dir)
  "Return the tab name prefix for project DIR."
  (concat (+project-tab-project-name dir) ":"))

;;;###autoload
(defun +project-tab-project-name (dir)
  "Return the display name for DIR's project."
  (let* ((root (directory-file-name
                (abbreviate-file-name
                 (project-root (project-current t dir)))))
         (remote-end (and (file-remote-p root)
                          (string-search ":" root (1+ (string-search ":" root 1)))))
         (remote-name (when remote-end
                        (substring root 1 (1+ remote-end))))
         (local-root (if remote-end
                         (substring root (1+ remote-end))
                       root))
         (project-name
          (if (string-prefix-p "~/" local-root)
              (let ((parts (split-string (substring local-root 2) "/" t)))
                (if (= (length parts) 1)
                    (concat "~/" (car parts))
                  (string-join (last parts 2) "/")))
            (string-join (last (split-string local-root "/" t) 2) "/"))))
    (if remote-name
        (concat remote-name project-name)
      project-name)))

;;;###autoload
(defun +project-tab--select-other ()
  "Switch to the most recently used other tab in the current project."
  (if (+project-tab--current-root)
      (if-let ((tab (seq-find #'+project-tab--tab-p
                              (tab-bar--tabs-recent))))
          (progn
            (tab-bar-select-tab (1+ (tab-bar--tab-index tab)))
            t)
        (+project-tab--handle-missing-other))
    (tab-bar-switch-to-recent-tab)))

;;;###autoload
(defun +project-tab--handle-missing-other ()
  "Handle a missing other tab for the current project."
  (if +project-tab-other-project-missing-command
      (progn
        (funcall +project-tab-other-project-missing-command)
        nil)
    (user-error "No other tabs for current project")))

;;;###autoload
(defun +project-tab--switch-to-project-and-command (dir)
  "Switch to DIR's project tab, creating one if needed, then dispatch a command."
  (project-remember-project (project-current t dir))
  (+project-tab-switch-to-project dir t)
  (+project-call-project-command dir))

;;;###autoload
(defun +project-tab--pin-current-project (dir)
  "Pin the current tab to the project containing DIR."
  (let* ((root (+project-tab--project-root dir))
         (prefix (+project-tab-project-prefix root))
         (tabs (funcall tab-bar-tabs-function))
         (tab (tab-bar--current-tab-find tabs)))
    (setf (alist-get '+project-tab-root (cdr tab)) root
          (alist-get '+project-tab-prefix (cdr tab)) prefix)
    (tab-bar-tabs-set tabs)
    (force-mode-line-update)))

;;;###autoload
(defun +project-tab--switch (arg create create-command)
  "Switch ARG project tabs from the current tab.
Positive ARG moves forward.  Negative ARG moves backward.
CREATE controls whether to create a tab when there is no other project tab.
CREATE-COMMAND is called when creating a tab."
  (let ((root (+project-tab--current-root)))
    (if (not root)
        (if (< arg 0)
            (tab-bar-switch-to-prev-tab (- arg))
          (tab-bar-switch-to-next-tab arg))
      (let* ((tabs (funcall tab-bar-tabs-function))
             (project-tabs (seq-filter #'+project-tab--tab-p tabs))
             (current-index (cl-position-if #'+project-tab--current-tab-p
                                            project-tabs)))
        (if (and current-index (> (length project-tabs) 1))
            (let* ((count (length project-tabs))
                   (target-index (mod (+ current-index arg) count))
                   (target-tab (nth target-index project-tabs)))
              (tab-bar-select-tab (1+ (tab-bar--tab-index target-tab))))
          (+project-tab--maybe-create create create-command))))))

;;;###autoload
(defun +project-tab--maybe-create (create create-command)
  "Create a project tab according to CREATE using CREATE-COMMAND."
  (let ((root (+project-tab--current-root)))
    (pcase create
      ('auto
       (funcall create-command)
       (when root
         (+project-tab--pin-current-project root))
       (setq repeat-map (make-sparse-keymap))
       create-command)
      ('prompt
       ;; TODO maybe i don't need this anymore?
       (repeat-exit)
       ;; this is a hack because repeat mode installs a hook that activates
       ;; a minibuffer hook that evaluates if the transient repeat map should be
       ;; active or not based on `this-command'. This circumvents the hook from running
       ;; to keep the expected behavior of `(repeat-exit)'.
       (setq repeat-map (make-sparse-keymap))
       (let ((y-or-n-p-use-read-key t))
         (if (y-or-n-p "Create new tab for current project? ")
             (progn
               (funcall create-command)
               (when root
                 (+project-tab--pin-current-project root))
               (setq repeat-map (make-sparse-keymap))
               create-command)
           (user-error "No other tabs for current project"))))
      (_ (user-error "No other tabs for current project")))))

;;;###autoload
(defun +project-tab--tabs ()
  "Return tabs belonging to the current project, or nil."
  (when (+project-tab--current-root)
    (seq-filter (lambda (tab)
                  (+project-tab--tab-p tab))
                (funcall tab-bar-tabs-function))))

;;;###autoload
(defun +project-tab--tab-p (tab &optional root)
  "Return non-nil when TAB belongs to ROOT.
ROOT defaults to the current project root metadata."
  (when-let ((root (or root (+project-tab--current-root)))
             (tab-root (+project-tab--tab-root tab)))
    (equal root tab-root)))

;;;###autoload
(defun +project-tab--tab-root (tab)
  "Return TAB's project root metadata."
  (alist-get '+project-tab-root tab))

;;;###autoload
(defun +project-tab--current-root ()
  "Return the current tab's project root metadata, or nil."
  (when-let ((tab (tab-bar--current-tab-find)))
    (+project-tab--tab-root tab)))

;;;###autoload
(defun +project-tab--current-project-root ()
  "Return the current tab or selected buffer's project root, or nil."
  (or (+project-tab--current-root)
      (when-let ((project (project-current nil)))
        (+project-tab--project-root (project-root project)))))

;;;###autoload
(defun +project-tab--project-root (dir)
  "Return the normalized project root containing DIR."
  (file-name-as-directory
   (expand-file-name
    (project-root (project-current t dir)))))

;;;###autoload
(defun +project-tab--current-tab-p (tab)
  "Return non-nil when TAB is the current tab."
  (eq (car tab) 'current-tab))

;;;###autoload
(defun +project-tab-name-tab-function ()
  "Generate a PROJECT:BUFFER tab name and pin new project metadata."
  (let* ((win (or (minibuffer-selected-window)
                  (and (window-minibuffer-p) (get-mru-window))))
         (buf (window-buffer win))
         (name (buffer-name buf))
         (tab (assq 'current-tab (frame-parameter nil 'tabs)))
         (metadata-root (and tab
                             (alist-get '+project-tab-root tab)))
         (metadata-prefix (and tab
                               (alist-get '+project-tab-prefix tab)))
         (project-root
          (unless metadata-root
            (when-let ((project (with-current-buffer buf
                                  (project-current))))
              (+project-tab--project-root (project-root project)))))
         (root (or metadata-root project-root))
         (project-prefix (and root
                              (not metadata-prefix)
                              (+project-tab-project-prefix root)))
         (prefix (or metadata-prefix project-prefix)))
    (when (and tab root)
      (unless metadata-root
        (setf (alist-get '+project-tab-root (cdr tab)) root))
      (unless metadata-prefix
        (setf (alist-get '+project-tab-prefix (cdr tab)) project-prefix)))
    (if prefix
        (format "%s%s" prefix name)
      name)))


(provide 'project-tab)
