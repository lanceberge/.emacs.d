;;; -*- lexical-binding: t -*-
(require 'elixir-utils)

(defvar elixir-web-mode-map
  (make-sparse-keymap)
  "Keymap for `elixir-web-mode'.")

(define-minor-mode elixir-web-mode
  "Elixir mode for web files."
  :lighter " Web"
  :keymap elixir-web-mode-map)

;;;###autoload
(defun +elixir-web-newline (arg)
  "Insert two newlines and put point between them if the point is
between two /html tags."
  (interactive "p")
  (if (and (eq (char-before (point)) ?>)
           (eq (char-after (point)) ?<))
      (progn
        (newline 2 t)
        (forward-line -1))
    (newline arg))
  (indent-according-to-mode))

;;;###autoload
(defun +elixir-web-maybe-enable ()
  "Enable `elixir-web-mode' if the module is '*Web.'"
  (when (+elixir--web-mode-p)
    (elixir-web-mode 1)))

;;;###autoload
(defun +elixir--web-mode-p ()
  "Return t if the current file is in a
Phoenix *_web directory (e.g., lib/my_app_web/...)."
  (when-let* ((rel-path (+project--buffer-relative-path))
              (parts (split-string rel-path "/"))
              (second-part (nth 1 parts)))
    (string-suffix-p "_web" second-part)))

;;;###autoload
(defun +elixir-web-maybe-close-tag ()
  "Automatically close html tags."
  (interactive)
  (with-undo-amalgamate
    (insert ">")
    (save-excursion
      (backward-char)
      (when (and (not (eq (char-before (point)) ?/)) ; don't pair self-closing tags
                 (not (eq (char-before (point)) ?|))
                 (not (and (looking-back "{[^}]*" (line-beginning-position))
                           (looking-at "[^{]*}"))) ; don't pair inside {} blocks
                 (looking-back "<\\([.:a-zA-Z_][a-zA-Z_.-]*\\)[^>]*" (line-beginning-position)))
        (let ((tag-name (match-string 1)))
          (unless (string-match-p "^/" tag-name)
            (forward-char)
            (insert (format "</%s>" tag-name))
            (backward-char (+ 2 (length tag-name)))))))))

(defun +elixir--inside-heex-sigil-p ()
  (save-excursion
    (let ((pos (point)))
      (if (re-search-backward "\"\"\"" nil t)
          (let ((back-pos (match-beginning 0)))
            (if (and (>= back-pos 2)
                     (string= (buffer-substring-no-properties (- back-pos 2) back-pos) "~H"))
                (progn
                  (goto-char pos)
                  (if (re-search-forward "\"\"\"\\|~H\"\"\"" nil t)
                      (let ((fwd-match (match-string 0)))
                        (string= fwd-match "\"\"\""))
                    (progn
                      nil)))
              (progn
                nil)))
        (progn
          nil)))))

(defun +elixir-web-comment ()
  (interactive)
  (let ((in-heex (+elixir--inside-heex-sigil-p)))
    (let ((comment-start (if in-heex "<!-- " comment-start))
          (comment-end   (if in-heex " -->"  comment-end)))
      (if (region-active-p)
          (call-interactively #'comment-dwim)
        (call-interactively #'comment-line)))))

;;;###autoload
(defun +elixir--maybe-setup-new-file ()
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
    (+elixir--setup-new-file)))

;;;###autoload
(defun +elixir--setup-new-file ()
  "Fill in component, livecomponent, or module snippets
on new elixir files based on their paths relative to the project root."
  (if (+elixir--web-mode-p)
      ;; TODO live page
      (let* ((subdirectories (split-string (+project--buffer-relative-path) "/"))
             (live-in-subdirectories (member "live" subdirectories))
             (components-in-subdirectories (member "components" subdirectories)))
        (cond ((and live-in-subdirectories components-in-subdirectories)
               (+yas-expand-snippet "live component"))
              (components-in-subdirectories
               (+yas-expand-snippet "component"))))
    (+yas-expand-snippet "elixir module")))

(provide 'elixir-web)
