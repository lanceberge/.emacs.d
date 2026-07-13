;;; jj-extras.el --- Jujutsu helpers -*- lexical-binding: t -*-

(require 'project)
(require 'subr-x)
(require 'transient)
(require 'vc)

(declare-function eshell-command "eshell" (command &optional to-current-buffer))
(declare-function majutsu-new "majutsu-new")

(autoload '+project-tab-new-project-command "project-tab" nil t)

(defgroup +jj nil
  "Jujutsu extensions."
  :group 'vc)

(defcustom +jj-git-clone-path "~/code"
  "Default parent directory for `+jj-git-clone'."
  :type 'directory
  :group '+jj)

(defcustom +jj-workspace-add-parent-dir "~/jj-workspaces/"
  "Parent directory for workspaces created by `majutsu-workspace-add'."
  :type 'directory
  :group '+jj)

(defcustom +jj-workspace-elixir-setup-command "mix deps.get"
  "Eshell command to run after adding an Elixir workspace.
The command is run asynchronously from the new workspace root."
  :type 'string
  :group '+jj)

(defvar +jj-post-new-hook nil
  "Hook run after `+jj-new' finishes.")

(defvar +jj-post-squash-hook nil
  "Hook run after `+jj-squash' finishes.")

(with-eval-after-load 'majutsu-core
  (transient-define-argument majutsu-transient-arg-ignore-immutable ()
    :description "Ignore immutable"
    :class 'transient-switch
    :shortarg "-I"
    :argument "--ignore-immutable"
    :init-value (lambda (obj) (oset obj value "--ignore-immutable"))))

;;;###autoload
(defun +jj-workspace-after-add ()
  "Run post-create setup and read a project command for a new workspace."
  (interactive)
  (let* ((project (project-current t default-directory))
         (root (file-name-as-directory (project-root project))))
    (project-remember-project project)
    (when (+jj-workspace-elixir-project-p project)
      (let ((default-directory root))
        (eshell-command (concat +jj-workspace-elixir-setup-command " &"))))
    (let ((default-directory root)
          (project-current-directory-override root))
      (+project-tab-new-project-command root))))

;;;###autoload
(defun +jj-workspace-add-dir ()
  "Default destination for `majutsu-workspace-add'."
  (let* ((root (or (vc-root-dir) default-directory))
         (repo-name (file-name-nondirectory
                     (directory-file-name (expand-file-name root)))))
    (expand-file-name (file-name-as-directory repo-name)
                      (expand-file-name +jj-workspace-add-parent-dir))))

;;;###autoload
(defun +jj-conflicted-files ()
  "Return absolute paths to files with jj conflicts."
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (lines (+jj--lines "resolve" "--list")))
    (or (mapcar (lambda (file)
                  (expand-file-name file root))
                (delq nil
                      (mapcar #'+jj--parse-conflict-line lines)))
        (+jj--conflicted-files-with-markers))))

;;;###autoload
(defun +jj-describe (message)
  "Run `jj describe -m MESSAGE'."
  (interactive "sDescribe: ")
  (let ((output (shell-command-to-string
                 (format "jj describe -m %s" (shell-quote-argument message)))))
    (message "%s" (string-trim output))))

;;;###autoload
(defun +jj-squash ()
  "Run `jj squash --ignore-immutable'."
  (interactive)
  (let ((output (shell-command-to-string
                 "jj squash --ignore-immutable")))
    (message "%s" (string-trim output))
    (run-hooks '+jj-post-squash-hook)))

;;;###autoload
(defun +jj-init ()
  "Initialize a colocated jj/git repository in the current project."
  (interactive)
  (let* ((project (project-current))
         (root (if project
                   (project-root project)
                 (read-directory-name "JJ init directory: ")))
         (default-directory root))
    (if (file-directory-p (expand-file-name ".jj" root))
        (message "Jujutsu repo already exists in %s" root)
      (message "%s"
               (shell-command-to-string
                "jj git init --colocate")))))

;;;###autoload
(defun +jj-new ()
  "Call `majutsu-new' and run `+jj-post-new-hook'."
  (interactive)
  (call-interactively #'majutsu-new)
  (run-hooks #'+jj-post-new-hook))

;;;###autoload
(defun +jj-git-clone (url dir)
  "Run `jj git clone URL' into DIR, then `project-find-file' in the new repo.
URL defaults to the system clipboard contents."
  (interactive
   (list (string-trim
          (or (gui-get-selection 'CLIPBOARD)
              (current-kill 0 t)
              (user-error "Clipboard is empty")))
         (let ((vertico-preselect 'prompt))
           (read-directory-name "Clone into directory: "
                                (file-name-as-directory
                                 (expand-file-name +jj-git-clone-path))))))
  (let* ((repo-name (file-name-base
                     (replace-regexp-in-string "\\.git/?\\'" "" url)))
         (parent (expand-file-name dir))
         (target (expand-file-name repo-name parent)))
    (unless (file-directory-p parent)
      (make-directory parent t))
    (let ((default-directory parent))
      (message "%s"
               (shell-command-to-string
                (format "jj git clone %s %s"
                        (shell-quote-argument url)
                        (shell-quote-argument target)))))
    (let ((default-directory (file-name-as-directory target)))
      (call-interactively #'project-find-file))))

;;;###autoload
(defun +jj-workspace-elixir-project-p (project)
  "Return non-nil when PROJECT contains an Elixir source file."
  (or (seq-some (lambda (file)
                  (string-suffix-p ".ex" file))
                (condition-case nil
                    (project-files project)
                  (error nil)))
      (seq-some (lambda (file)
                  (string-suffix-p ".ex" file))
                (condition-case nil
                    (directory-files-recursively
                     (project-root project) "\\.ex\\'" nil t)
                  (error nil)))))

;;;###autoload
(defun +jj--parse-conflict-line (line)
  "Return the file path from a `jj resolve --list' LINE."
  (when (string-match
         "[ \t]+[0-9]+-sided conflict\\(?: including .*\\)?\\(?:\r\\)?\\'"
         line)
    (string-trim-right (substring line 0 (match-beginning 0)))))

;;;###autoload
(defun +jj--conflicted-files-with-markers ()
  "Return jj-controlled files containing conflict markers."
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (files (+jj--lines "file" "list")))
    (mapcar (lambda (file)
              (expand-file-name file root))
            (+jj--files-with-conflict-markers files))))

;;;###autoload
(defun +jj--files-with-conflict-markers (files)
  "Return relative FILES containing conflict start markers."
  (when files
    (with-temp-buffer
      (let ((status (apply #'process-file
                           "rg" nil t nil
                           "--files-with-matches"
                           "--no-messages"
                           "--" "^<<<<<<<" files)))
        (if (memq status '(0 1))
            (split-string (buffer-string) "\n" t)
          nil)))))

;;;###autoload
(defun +jj--lines (&rest args)
  "Run jj with ARGS and return output lines."
  (with-temp-buffer
    (let ((status (apply #'process-file "jj" nil t nil "--no-pager" args)))
      (if (zerop status)
          (split-string (buffer-string) "\n" t)
        nil))))

;;;###autoload
(defun +majutsu-forget-command (directory)
  (let ((project-root (file-name-as-directory
                       (expand-file-name directory))))
    (when-let* (((and (fboundp 'project-current)
                      (fboundp 'project-kill-buffers)))
                (project (project-current nil project-root)))
      (project-kill-buffers t project))
    (when (fboundp 'project-forget-project)
      (project-forget-project project-root))
    (when (file-directory-p project-root)
      (delete-directory project-root t))))

(provide 'jj-extras)
;;; jj-extras.el ends here
