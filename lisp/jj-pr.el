;;; jj-pr.el --- Jujutsu pull request commands -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup +jj-pr nil
  "Pull requests from jj repositories."
  :group 'vc)

(defcustom +jj-pr-create-command "jj_pr"
  "Command used to push a jj bookmark and create a pull request."
  :type 'string
  :group '+jj-pr)

(defcustom +jj-pr-latest-bookmark-command "jj_latest_bookmark"
  "Command used to find the nearest jj bookmark."
  :type 'string
  :group '+jj-pr)

(defcustom +jj-pr-gh-command "gh"
  "GitHub CLI command used to inspect and edit pull requests."
  :type 'string
  :group '+jj-pr)

(defvar-local +jj-pr-branch nil)
(defvar-local +jj-pr-title nil)
(defvar-local +jj-pr-directory nil)
(defvar-local +jj-pr-number nil)
(defvar-local +jj-pr-draft nil)

;;;###autoload
(defun +jj-pr-create (arg)
  "Create a pull request for the current jj repository.
With prefix ARG, edit the PR body in a separate buffer first."
  (interactive "P")
  (let* ((root (+jj-pr--root))
         (default-directory root)
         (default-branch (+jj-pr--default-branch))
         (branch (read-string (format "PR branch [%s]: " default-branch)
                              nil nil default-branch))
         (default-title (+jj-pr--default-title))
         (title (read-string (format "PR title%s"
                                     (if default-title
                                         (format " [%s]" default-title)
                                       ""))
                             nil nil default-title))
         (draft (y-or-n-p "Draft? ")))
    (when (string-empty-p title)
      (user-error "PR title cannot be empty"))
    (if arg
        (+jj-pr--open-body root branch title nil "" draft)
      (+jj-pr--create root branch title nil nil draft))))

;;;###autoload
(defun +jj-pr-edit-description ()
  "Edit the PR body for the nearest jj bookmark."
  (interactive)
  (let* ((root (+jj-pr--root))
         (default-directory root)
         (branch (+jj-pr--default-branch))
         (pr (+jj-pr--details root branch)))
    (+jj-pr--open-body root branch nil (alist-get 'number pr)
                       (or (alist-get 'body pr) ""))))

;;;###autoload
(defun +jj-pr-submit-body ()
  "Create or update the PR using the current body buffer."
  (interactive)
  (unless +jj-pr-body-mode
    (user-error "Not in a JJ PR body buffer"))
  (let ((body (buffer-substring-no-properties (point-min) (point-max)))
        (source-buffer (current-buffer)))
    (setq buffer-read-only t)
    (condition-case error
        (if +jj-pr-number
            (+jj-pr--run +jj-pr-directory
                         (list +jj-pr-gh-command "pr" "edit"
                               (number-to-string +jj-pr-number) "--body" body)
                         source-buffer)
          (+jj-pr--create +jj-pr-directory +jj-pr-branch +jj-pr-title body
                          source-buffer +jj-pr-draft))
      (error
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (setq buffer-read-only nil)))
       (signal (car error) (cdr error))))))

;;;###autoload
(defun +jj-pr-cancel-body ()
  "Discard the current PR body without creating a PR."
  (interactive)
  (unless +jj-pr-body-mode
    (user-error "Not in a JJ PR body buffer"))
  (set-buffer-modified-p nil)
  (quit-window t))

;;;###autoload
(defun +jj-pr--open-body (directory branch title number body &optional draft)
  "Open a Markdown BODY buffer for a PR in DIRECTORY.
BRANCH and TITLE are used for creation; NUMBER identifies a PR to edit."
  (require 'markdown-mode)
  (let ((buffer (generate-new-buffer "*jj PR body*")))
    (with-current-buffer buffer
      (gfm-mode)
      (unless markdown-hide-markup
        (markdown-toggle-markup-hiding))
      (+jj-pr-body-mode 1)
      (setq-local +jj-pr-branch branch
                  +jj-pr-title title
                  +jj-pr-directory directory
                  +jj-pr-number number
                  +jj-pr-draft draft
                  default-directory directory)
      (insert body)
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)
    (message "Edit the PR body, then C-c C-c to submit or C-c C-k to cancel")))

;;;###autoload
(defun +jj-pr--root ()
  "Return the root of the current jj repository."
  (let ((root (locate-dominating-file default-directory ".jj")))
    (unless root
      (user-error "Not in a jj repository"))
    (file-name-as-directory (expand-file-name root))))

;;;###autoload
(defun +jj-pr--create (directory branch title body &optional source-buffer draft)
  "Run jj_pr in DIRECTORY with BRANCH, TITLE, and optional BODY."
  (+jj-pr--run directory
               (append (list +jj-pr-create-command "-b" branch "--fill" "--title" title)
                       (when body (list "--body" body))
                       (when draft (list "--draft")))
               source-buffer))

;;;###autoload
(defun +jj-pr--run (directory command &optional source-buffer)
  "Run PR COMMAND asynchronously in DIRECTORY.
Keep SOURCE-BUFFER open for a retry if the command fails."
  (let ((default-directory directory)
        (process-environment (+jj-pr--environment))
        (output ""))
    (make-process
     :name "jj-pr"
     :buffer nil
     :command command
     :connection-type 'pipe
     :filter (lambda (_process chunk)
               (setq output (concat output chunk)))
     :sentinel (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   (let ((success (and (eq (process-status process) 'exit)
                                       (zerop (process-exit-status process)))))
                     (when (buffer-live-p source-buffer)
                       (if success
                           (progn
                             (with-current-buffer source-buffer
                               (set-buffer-modified-p nil))
                             (if-let* ((window (get-buffer-window source-buffer t)))
                                 (quit-window t window)
                               (kill-buffer source-buffer)))
                         (with-current-buffer source-buffer
                           (setq buffer-read-only nil))))
                     (if success
                         (message "%s"
                                  (if (string-match "https?://[^[:space:]\e]+" output)
                                      (format "Pull request: %s" (match-string 0 output))
                                    "PR command succeeded"))
                       (message "PR command failed: %s" (string-trim event)))))))))

;;;###autoload
(defun +jj-pr--details (directory branch)
  "Return the GitHub PR for BRANCH in DIRECTORY."
  (let ((default-directory directory)
        (process-environment (+jj-pr--environment)))
    (with-temp-buffer
      (let ((status (process-file +jj-pr-gh-command nil t nil "pr" "view" branch
                                  "--json" "number,body")))
        (unless (eq status 0)
          (user-error "gh pr view: %s" (string-trim (buffer-string))))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist)))))

;;;###autoload
(defun +jj-pr--environment ()
  "Authenticate gh and return an environment without GH_TOKEN."
  (let ((process-environment
         (cl-remove-if (lambda (entry)
                         (or (equal entry "GH_TOKEN")
                             (string-prefix-p "GH_TOKEN=" entry)))
                       process-environment)))
    (with-temp-buffer
      (insert-file-contents-literally (expand-file-name "~/secrets/gh_token"))
      (base64-decode-region (point-min) (point-max))
      (unless (eq 0 (call-process-region (point-min) (point-max)
                                         +jj-pr-gh-command nil nil nil
                                         "auth" "login" "--with-token"))
        (user-error "gh authentication failed")))
    process-environment))

;;;###autoload
(defun +jj-pr--default-title ()
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
(defun +jj-pr--default-branch ()
  "Return the branch proposed by jj_latest_bookmark."
  (with-temp-buffer
    (let ((status (process-file +jj-pr-latest-bookmark-command nil t)))
      (unless (zerop status)
        (user-error "%s" (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defvar-keymap +jj-pr-body-mode-map
  "C-c C-c" #'+jj-pr-submit-body
  "C-c C-k" #'+jj-pr-cancel-body)

;;;###autoload
(define-minor-mode +jj-pr-body-mode
  "Minor mode for submitting or canceling a PR body edit.")

(provide 'jj-pr)
;;; jj-pr.el ends here
