;;; consult-gh-jj-pr.el ---  GitHub pull requests using jj -*- lexical-binding: t -*-

(require 'json)
(require 'subr-x)
(require 'consult-gh)

(defgroup +consult-gh-jj-pr nil
  "Pull requests from jj repositories with consult-gh."
  :group 'vc)

(defcustom +consult-gh-jj-pr-latest-bookmark-command "jj_latest_bookmark"
  "Command used to find the nearest jj bookmark."
  :type 'string
  :group '+consult-gh-jj-pr)

;;;###autoload
(defun +consult-gh-jj-pr-create (arg)
  "Create a PR from the latest jj bookmark.
With prefix ARG, prompt for a remote head branch without an initial value."
  (interactive "P")
  (let* ((default-directory (+consult-gh-jj-pr--root))
         (repo (+consult-gh-jj-pr--repo))
         (branch (+consult-gh-jj-pr--branch repo arg)))
    (consult-gh-pr-create repo (+consult-gh-jj-pr--default-title)
                          nil nil repo branch)))

;;;###autoload
(defun +consult-gh-jj-pr-edit (arg)
  "Edit the open PR for the latest jj bookmark.
With prefix ARG, prompt for a remote head branch without an initial value."
  (interactive "P")
  (let* ((default-directory (+consult-gh-jj-pr--root))
         (repo (+consult-gh-jj-pr--repo))
         (branch (+consult-gh-jj-pr--branch repo arg)))
    (consult-gh-pr-edit (+consult-gh-jj-pr--select repo branch))))

;;;###autoload
(defun +consult-gh-jj-pr-close (arg)
  "Close the open PR for the latest jj bookmark.
With prefix ARG, prompt for a remote head branch without an initial value."
  (interactive "P")
  (let* ((default-directory (+consult-gh-jj-pr--root))
         (repo (+consult-gh-jj-pr--repo))
         (branch (+consult-gh-jj-pr--branch repo arg)))
    (consult-gh-pr-close (+consult-gh-jj-pr--select repo branch))))

;;;###autoload
(defun +consult-gh-jj-pr--root ()
  "Return the root of the current jj repository."
  (let ((root (locate-dominating-file default-directory ".jj")))
    (unless root
      (user-error "Not in a jj repository"))
    (file-name-as-directory (expand-file-name root))))

;;;###autoload
(defun +consult-gh-jj-pr--repo (&optional prompt)
  "Return the current GitHub repo, or prompt when PROMPT is non-nil."
  (or (unless prompt (consult-gh--get-repo-from-directory))
      (get-text-property 0 :repo
                         (consult-gh-search-repos nil t "Select GitHub repo: "))
      (user-error "No GitHub repository selected")))

;;;###autoload
(defun +consult-gh-jj-pr--branch (repo prompt)
  "Return the latest jj bookmark, or prompt for a branch of REPO."
  (if prompt
      (substring-no-properties
       (consult-gh--read-branch repo nil "Head branch: " t nil))
    (+consult-gh-jj-pr--latest-bookmark)))

;;;###autoload
(defun +consult-gh-jj-pr--select (repo branch)
  "Return the open PR on BRANCH in REPO, prompting if needed."
  (pcase-let* ((`(,status ,response)
                (consult-gh--call-process "pr" "list" "--repo" repo
                                          "--head" branch "--state" "open"
                                          "--json" "number,author"))
               (_ (unless (eq status 0)
                    (user-error "Could not list PRs for %s: %s"
                                repo (string-trim response))))
               (prs (json-parse-string response :object-type 'alist
                                       :array-type 'list)))
    (pcase prs
      (`(,pr)
       (let ((number (alist-get 'number pr)))
         (propertize (format "%s#%s" repo number)
                     :repo repo :baserepo repo :number number
                     :type "pr" :state "OPEN"
                     :author (alist-get 'login (alist-get 'author pr)))))
      (_
       (let* ((repo (if prs repo (+consult-gh-jj-pr--repo t)))
              (initial (if prs (format "%s --head %s" repo branch) repo)))
         (or (consult-gh-pr-list initial t "Select PR: ")
             (user-error "No pull request selected")))))))

;;;###autoload
(defun +consult-gh-jj-pr--default-title ()
  "Return the first line of the oldest description in the stack up to @-."
  (with-temp-buffer
    (when (zerop (process-file "jj" nil '(t nil) nil
                               "log" "--no-graph" "--ignore-working-copy"
                               "-r" "roots(trunk()..@-)" "-n" "1"
                               "-T" "description.first_line()"))
      (let ((title (string-trim (buffer-string))))
        (unless (string-empty-p title)
          title)))))

;;;###autoload
(defun +consult-gh-jj-pr--latest-bookmark ()
  "Return the bookmark proposed by jj_latest_bookmark."
  (with-temp-buffer
    (let ((status (process-file +consult-gh-jj-pr-latest-bookmark-command
                                nil t)))
      (unless (zerop status)
        (user-error "%s" (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(provide 'consult-gh-jj-pr)
;;; consult-gh-jj-pr.el ends here
