;;; -*- lexical-binding: t -*-
;;;###autoload
(defun +elixir-newline (arg)
  "Insert two newlines and put point between them if the point is between two xml/html tags."
  (interactive "p")
  (if (and (eq (char-before (point)) ?>)
           (eq (char-after (point)) ?<))
      (progn
        (newline 2 t)
        (previous-line)
        (indent-according-to-mode))
    (newline arg t)))

;;;###autoload
(defun maybe-elixir-web-mode ()
  "Enable `elixir-web-mode' if the module is '*Web.'"
  (when (+elixir--web-mode-p)
    (elixir-web-mode 1)))

;;;###autoload
(defun +elixir--web-mode-p ()
  "Return t if the current file is in a Phoenix *_web directory (e.g., lib/my_app_web/...)."
  (when-let* ((rel-path (+project--buffer-relative-path))
              (parts (split-string rel-path "/"))
              (second-part (nth 1 parts)))
    (string-suffix-p "_web" second-part)))

;;;###autoload
(defun +maybe-close-tag ()
  "Automatically close html tags."
  (interactive)
  (with-undo-amalgamate
    (insert ">")
    (save-excursion
      (backward-char)
      (when (and (not (eq (char-before (point)) ?/)) ; don't pair self-closing tags
                 (not (eq (char-before (point)) ?|))
                 (looking-back "<\\([.:a-zA-Z_][a-zA-Z_.-]*\\)[^>]*" (line-beginning-position)))
        (let ((tag-name (match-string 1)))
          (unless (string-match-p "^/" tag-name)
            (forward-char)
            (insert (format "</%s>" tag-name))
            (backward-char (+ 2 (length tag-name)))))))))

;;;###autoload
;; TODO
(defun +elixir-create-schema ()
  (interactive)
  ())

;;;###autoload
;; TODO hook to renaming .ex files
;; TODO rename functional component function names as well
(defun +elixir-rename-module ()
  (interactive)
  (let* ((current-module-name (+elixir--current-module-name))
         (updated-module-name (+elixir--module-name-from-file))
         (current-module-base (car (last (split-string current-module-name "\\." t))))
         (updated-module-base (car (last (split-string updated-module-name "\\." t)))))
    (when (buffer-modified-p)
      (save-buffer))
    (unless (string-equal current-module-name updated-module-name)
      (+project-replace-regex
       (concat "\\b" (regexp-quote current-module-name) "\\b")
       updated-module-name)
      "*.ex")
    ;; Replace struct names
    (when (and (not (string-equal current-module-name updated-module-name))
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^[[:space:]]*defstruct" nil t)))
      (+project-replace-regex
       (concat "\\b" current-module-base ".")
       (concat updated-module-base ".")
       "*.ex")
      (+project-replace-regex
       (concat "%" current-module-base "{")
       (concat "%" updated-module-base "{")
       "*.ex"))
    (revert-buffer nil t t)))

;;;###autoload
(defun +elixir--current-module-name ()
  "Return the current module name."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*defmodule[ \t]+\\([A-Za-z0-9.]+\\)" nil t)
      (let ((module-name (match-string-no-properties 1)))
        module-name))))

;;;###autoload
(defun +elixir--module-name-from-file ()
  "Generate Elixir module name from current file path relative to project lib/ directory."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-root (project-root (project-current t)))
         (lib-path (expand-file-name "lib/" project-root))
         (relative-path (file-relative-name file-path lib-path))
         (path-without-ext (file-name-sans-extension relative-path))
         (parts (split-string path-without-ext "/" t))
         (module-parts (mapcar (lambda (part)
                                 (mapconcat (lambda (word)
                                              (concat (upcase (substring word 0 1))
                                                      (substring word 1)))
                                            (split-string part "_")
                                            ""))
                               parts))
         (module-name (mapconcat 'identity module-parts ".")))
    module-name))

;;;###autoload
(defun +elixir--component-name-from-file ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

;;;###autoload
(defun +elixir-mode ()
  (interactive)
  (beginend-prog-mode -1))

;;;###autoload
(defun +elixir--maybe-setup-new-file ()
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
    (if (bound-and-true-p yas-minor-mode)
        (+elixir--setup-new-file)
      (add-hook 'yas-minor-mode-hook #'+elixir--setup-new-file nil t))))

;;;###autoload
(defun +elixir--setup-new-file ()
  "Fill in component, livecomponent, or module snippets on new elixir files based on their paths
relative to the project root."
  (require 'yasnippet)
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
