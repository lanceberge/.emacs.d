;;; consult-buffer-extras.el --- Consult buffer commands -*- lexical-binding: t -*-

(require 'consult-extras)
(require 'modal)
(require 'project)
(require 'seq)

;;;###autoload
(defun +consult-buffer-agent-shell (&optional arg)
  "Switch to an agent shell buffer, or create one with ARG."
  (interactive "P")
  (+consult-buffer--project-dwim "Agent" #'agent-shell-openai-start-codex nil arg))

;;;###autoload
(defun +consult-buffer-project-eshell-new (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (+consult-buffer-project-eshell 1)))

;;;###autoload
(defun +consult-buffer-project-eshell (&optional arg)
  "Switch to a project eshell buffer, or create one with ARG."
  (interactive "P")
  (require 'eshell)
  (+consult-buffer--project-dwim
   #'+consult-buffer-project-eshell-buffer
   #'+consult-buffer-create-project-eshell
   t
   arg)
  (+insert-mode 1))

;;;###autoload
(defun +consult-buffer-project-dwim
    (&optional initial create-fn case-insensitive force-create)
  "Switch to a matching project buffer or run `+consult-project-buffer'.

When INITIAL is nil, run `+consult-project-buffer'.  When INITIAL matches
exactly one project buffer, switch to it.  If INITIAL matches no project
buffers, call CREATE-FN.  CASE-INSENSITIVE defaults to t.  With FORCE-CREATE,
call CREATE-FN."
  (interactive "P")
  (if force-create
      (+consult-buffer--create-new create-fn)
    (if (not initial)
        (+consult-project-buffer)
      (if (not (project-current nil))
          (+consult-buffer-dwim initial create-fn case-insensitive)
        (consult--with-project
          (let ((project-matches
                 (+consult-buffer--matching-project-buffers
                  initial case-insensitive)))
            (cond
             ((null project-matches)
              (+consult-buffer--create-new create-fn))
             ((null (cdr project-matches))
              (consult--buffer-action (car project-matches)))
             (t
              (+consult-project-buffer initial)))))))))

;;;###autoload
(defun +consult-buffer-dwim
    (&optional initial create-fn case-insensitive force-create)
  "Switch to a matching buffer or run `consult-buffer'.

When INITIAL is nil, run `consult-buffer'.  When INITIAL matches exactly one
buffer, switch to it.  When INITIAL matches no buffers, call CREATE-FN.
Otherwise run Consult with INITIAL as the starting input.  CASE-INSENSITIVE
defaults to t.  With FORCE-CREATE, call CREATE-FN."
  (interactive "P")
  (if force-create
      (+consult-buffer--create-new create-fn)
    (if (not initial)
        (consult-buffer)
      (let ((matches
             (+consult-buffer--matching-buffers
              initial (buffer-list) case-insensitive)))
        (if (null (cdr matches))
            (if matches
                (consult--buffer-action (car matches))
              (if create-fn
                  (+consult-buffer--create-new create-fn)
                (+consult--buffer consult-buffer-sources ?b initial)))
          (+consult--buffer consult-buffer-sources ?b initial))))))

;;;###autoload
(defun +consult-buffer-project-eshell-buffer ()
  "Return the current project's eshell buffer name."
  (project-prefixed-buffer-name "eshell"))

;;;###autoload
(defun +consult-buffer-create-project-eshell ()
  "Create a new project eshell buffer."
  (let ((current-prefix-arg '(4)))
    (project-eshell)))

;;;###autoload
(defun +consult-buffer--project-dwim
    (buffer create-fn case-insensitive force-create)
  "Run project buffer DWIM for BUFFER.

BUFFER is either a string or a function returning a string.  CREATE-FN is a
zero-argument function that creates a new buffer.  CASE-INSENSITIVE controls
matching.  With FORCE-CREATE, call CREATE-FN immediately."
  (+consult-buffer-project-dwim
   (+consult-buffer--resolve-buffer buffer)
   create-fn
   case-insensitive
   force-create))

;;;###autoload
(defun +consult-buffer--matching-project-buffers
    (initial &optional case-insensitive)
  "Return project buffers whose names match INITIAL."
  (when-let* ((root (consult--project-root)))
    (+consult-buffer--matching-buffers
     initial
     (consult--buffer-query :sort 'visibility
                            :directory root)
     case-insensitive)))

;;;###autoload
(defun +consult-buffer--matching-buffers
    (initial buffers &optional case-insensitive)
  "Return BUFFERS whose names match INITIAL case-insensitively.
BUFFERS may be nil."
  (let ((case-fold-search (not (eq case-insensitive nil)))
        (regexp (concat ".*" (regexp-quote initial) ".*")))
    (seq-filter
     (lambda (buffer)
       (string-match-p regexp (buffer-name buffer)))
     buffers)))

;;;###autoload
(defun +consult-buffer--create-new (create-fn)
  "Call CREATE-FN."
  (if create-fn
      (funcall create-fn)
    (user-error "No create command configured")))

;;;###autoload
(defun +consult-buffer--resolve-buffer (buffer)
  "Return the buffer match string for BUFFER."
  (if (functionp buffer)
      (funcall buffer)
    buffer))

(provide 'consult-buffer-extras)
;;; consult-buffer-extras.el ends here
