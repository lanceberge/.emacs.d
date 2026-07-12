;;; -*- lexical-binding: t -*-

(require 'yasnippet)
(require 'project)
(require 'compile)
(require 'editor-lisp)
(require 'treesit)
(require 'elixir-ts-mode)

;;;###autoload
(defun +elixir--module-name-from-file ()
  "Generate Elixir module name from current file path relative
to project lib/ directory."
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
;; TODO
(defun +elixir-create-schema ()
  (interactive)
  ())

;;;###autoload
(defun +elixir--current-module-name ()
  "Return the current module name."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*defmodule[ \t]+\\([A-Za-z0-9.]+\\)" nil t)
      (let ((module-name (match-string-no-properties 1)))
        module-name))))

(defconst +elixir-mix-compilation-regexp
  (rx line-start
      (zero-or-more (not "\n"))
      (or line-start blank)
      (group (or "lib" "test")
             "/"
             (one-or-more (not (any ":\n")))
             ".ex"
             (optional "s"))
      ":"
      (group (one-or-more digit))
      (optional ":" (group (one-or-more digit)))
      (or ":" line-end))
  "Match existing local Elixir project paths in Mix compilation output.")

;;;###autoload
(defun +compilation-setup-mix-error-regexp (process)
  "Use Mix-specific compilation parsing in PROCESS' buffer."
  (when-let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (when (+compilation-mix-command-p)
        (setq-local compilation-error-regexp-alist-alist
                    (cons `(+elixir-mix
                            ,+elixir-mix-compilation-regexp
                            +elixir-compilation-file 2 3)
                          (default-value 'compilation-error-regexp-alist-alist)))
        (setq-local compilation-error-regexp-alist
                    (cons '+elixir-mix
                          (default-value 'compilation-error-regexp-alist)))))))

;;;###autoload
(defun +elixir-compilation-file ()
  "Return the current Mix compilation match if it exists on disk."
  (let* ((path (match-string-no-properties 1))
         (file (and path (expand-file-name path default-directory))))
    (when (and file (file-exists-p file))
      path)))

;;;###autoload
(defun +compilation-mix-command-p ()
  "Return non-nil when the current compilation command invokes Mix."
  (when-let ((command (car-safe compilation-arguments)))
    (string-match-p (rx (or string-start (not (any alnum "_" "-")))
                        "mix"
                        (or string-end (not (any alnum "_" "-"))))
                    command)))

(defun +project--buffer-relative-path ()
  "Return the buffer's path relative to the project-root"
  (require 'project)
  (when-let ((project (project-current t))
             (root (project-root project))
             (file (buffer-file-name)))
    (file-relative-name file root)))

;;;###autoload
(defun +elixir-ts-forward-sexp (&optional arg)
  "Move across syntactic Elixir sexps.
Embedded HEEx uses the package-provided sexp behavior."
  (if (eq (treesit-language-at (point)) 'heex)
      (if (+elixir-ts--heex-expression-node-p (treesit-node-at (point)))
          (forward-sexp-default-function arg)
        (elixir-ts--forward-sexp arg))
    (treesit-forward-sexp arg)))

;;;###autoload
(defun +elixir-ts-use-syntax-sexp ()
  "Use syntactic Tree-sitter sexp movement in `elixir-ts-mode'."
  (setq-local treesit-thing-settings
              `((elixir
                 (sexp ,(lambda (node)
                          (treesit-node-check node 'named))))))
  (setq-local treesit-sexp-thing 'sexp)
  (setq-local forward-sexp-function #'+elixir-ts-forward-sexp))

;;;###autoload
(defun +elixir-ts--heex-expression-node-p (node)
  "Return non-nil when NODE is part of a HEEx expression."
  (or (equal (treesit-node-type node) "expression_value")
      (when-let* ((parent (treesit-node-parent node)))
        (equal (treesit-node-type parent) "expression"))))


(provide 'elixir-utils)
