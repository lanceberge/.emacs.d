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
  (let ((prefix (+project-tab--current-prefix)))
    (tab-bar-new-tab arg)
    (when prefix
      (+project-tab--pin-current-prefix prefix))))

;;;###autoload
(defun +project-tab-new-project-command (&optional dir)
  "Run a project command for DIR, displaying the resulting buffer in a new tab.
DIR defaults to the current project root.
The following commands are available:
\\{project-prefix-map}"
  (interactive)
  (let* ((dir (or dir (project-root (project-current t))))
         (prefix (+project-tab-project-prefix dir)))
    (tab-bar-new-tab)
    (+project-tab--pin-current-prefix prefix)
    (+project-call-project-command dir)))

;;;###autoload
(defun +project-tab-switch-project-command (dir)
  "Switch to DIR's most recent project tab, then read and run a project command."
  (interactive (list (funcall project-prompter)))
  (+project-tab--switch-to-project-and-command dir))

;;;###autoload
(defun +project-tab-switch-other-project-command (dir)
  "Switch to the other project's most recent tab, then read and run a command."
  (interactive (list (+project-last-opened-other-project-root)))
  (+project-tab--switch-to-project-and-command dir))

;;;###autoload
(defun +project-tab-other-project-command ()
  "Switch to the most recently used other project tab and read a project command."
  (interactive)
  (let ((dir (when (project-current nil)
               (project-root (project-current t)))))
    (when (+project-tab--select-other)
      (+project-call-project-command dir))))

;;;###autoload
(defun +project-tab-next-project-command (&optional arg)
  "Switch to the next project tab, then read a project command."
  (interactive "p")
  (+project-tab-next arg)
  (+project-call-project-command))

;;;###autoload
(defun +project-tab-prev-project-command (&optional arg)
  "Switch to the previous project tab, then read a project command."
  (interactive "p")
  (+project-tab-prev arg)
  (+project-call-project-command))

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
  (let ((prefix (+project-tab-project-prefix dir)))
    (if-let ((tab (seq-find (lambda (tab)
                              (+project-tab--tab-p tab prefix))
                            (tab-bar--tabs-recent))))
        (tab-bar-select-tab (1+ (tab-bar--tab-index tab)))
      (when create
        (tab-bar-new-tab)
        (+project-tab--pin-current-prefix prefix)))))

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
  (if (+project-tab--current-prefix)
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
(defun +project-tab--pin-current-prefix (prefix)
  "Pin the current tab to PREFIX."
  (when-let ((tab (tab-bar--current-tab-find)))
    (setf (alist-get '+project-tab-prefix tab) prefix
          (alist-get 'explicit-name tab) nil)
    (force-mode-line-update)))

;;;###autoload
(defun +project-tab--switch (arg create create-command)
  "Switch ARG project tabs from the current tab.
Positive ARG moves forward.  Negative ARG moves backward.
CREATE controls whether to create a tab when there is no other project tab.
CREATE-COMMAND is called when creating a tab."
  (let ((prefix (+project-tab--current-prefix)))
    (if (not prefix)
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
  (let ((prefix (+project-tab--current-prefix)))
    (pcase create
      ('auto
       (funcall create-command)
       (when prefix
         (+project-tab--pin-current-prefix prefix))
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
               (when prefix
                 (+project-tab--pin-current-prefix prefix))
               (setq repeat-map (make-sparse-keymap))
               create-command)
           (user-error "No other tabs for current project"))))
      (_ (user-error "No other tabs for current project")))))

;;;###autoload
(defun +project-tab--tabs ()
  "Return tabs whose names belong to the current project, or nil."
  (when (+project-tab--current-prefix)
    (seq-filter (lambda (tab)
                  (+project-tab--tab-p tab))
                (funcall tab-bar-tabs-function))))

;;;###autoload
(defun +project-tab--tab-p (tab &optional prefix)
  "Return non-nil when TAB belongs to PREFIX.
PREFIX defaults to the current project prefix."
  (when-let ((prefix (or prefix (+project-tab--current-prefix)))
             (tab-prefix (+project-tab--tab-prefix tab)))
    (equal prefix tab-prefix)))

;;;###autoload
(defun +project-tab--tab-prefix (tab)
  "Return TAB's pinned or inferred project prefix."
  (or (alist-get '+project-tab-prefix tab)
      (when-let* ((name (alist-get 'name tab))
                  (index (string-search ":" name))
                  ((not (zerop index))))
        (substring name 0 (1+ index)))))

;;;###autoload
(defun +project-tab--current-prefix ()
  "Return the tab name prefix for the current project tab, or nil."
  (or (when-let ((tab (tab-bar--current-tab-find)))
        (alist-get '+project-tab-prefix tab))
      (when-let ((project (project-current nil)))
        (+project-tab-project-prefix (project-root project)))
      (+project-tab--current-tab-prefix)))

;;;###autoload
(defun +project-tab--current-tab-prefix ()
  "Return the project prefix from the current tab name, or nil."
  (when-let* ((tab (tab-bar--current-tab-find))
              (name (alist-get 'name tab))
              (index (string-search ":" name))
              ((not (zerop index))))
    (substring name 0 (1+ index))))

;;;###autoload
(defun +project-tab--current-tab-p (tab)
  "Return non-nil when TAB is the current tab."
  (eq (car tab) 'current-tab))

;;;###autoload
(defun +project-tab-name-tab-function ()
  "Generate tab name as pinned PROJECT:BUFFER from the selected window's buffer."
  (let* ((win (or (minibuffer-selected-window)
                  (and (window-minibuffer-p) (get-mru-window))))
         (buf (window-buffer win))
         (name (buffer-name buf))
         (prefix (or (when-let ((tab (assq 'current-tab
                                           (frame-parameter nil 'tabs))))
                       (alist-get '+project-tab-prefix tab))
                     (when-let ((project (with-current-buffer buf
                                           (project-current))))
                       (+project-tab-project-prefix (project-root project))))))
    (if prefix
        (format "%s%s" prefix name)
      name)))


(provide 'project-tab)
