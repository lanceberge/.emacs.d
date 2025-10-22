;;; -*- lexical-binding: t -*-
(defvar elixir-web-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ">") #'+maybe-close-tag)
    map)
  "Keymap for `elixir-web-mode'.")

(define-minor-mode elixir-web-mode
  "Elixir mode for web files."
  :lighter " Web"
  :keymap elixir-web-mode-map)

;;;###autoload
(defun maybe-elixir-web-mode ()
  "Enable `elixir-web-mode' if the module is '*Web.'"
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^defmodule [^.]*Web")
      (elixir-web-mode 1))))

;;;###autoload
(defun +maybe-close-tag ()
  "auto close html tags"
  (interactive)
  (with-undo-amalgamate
    (insert ">")
    (save-excursion
      (backward-char)
      (when (looking-back "<\\([.:a-zA-Z_][a-zA-Z_.-]*\\)[^>]*" (line-beginning-position))
        (let ((tag-name (match-string 1)))
          (unless (string-match-p "^/" tag-name)
            (forward-char)
            (insert (format "</%s>" tag-name))
            (backward-char (+ 2 (length tag-name)))))))))

(defvar +elixir-mode-map (make-sparse-keymap))

(use-package elixir-mode
  ;; :hook
  ;; ((elixir-mode elixir-ts-mode) . +setup-elixir-map)
  :bind
  (:map +elixir-mode-map
        ("a" . #'forward-char))

  :defer t)

;; (defun +setup-elixir-map ()
;;   "Set up leader key bindings for elixir-mode."
;;   (define-key +elixir-mode-map (kbd "C-c l") +leader-map)
;;   (define-key +leader2-map (kbd "m") +elixir-mode-map))

;;;###autoload
(defun +elixir-create-schema ()
  (interactive)
  ())

;;;###autoload
(defun elixir-module-name-from-file ()
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


(use-package elixir-ts-mode
  :hook ((elixir-ts-mode . maybe-elixir-web-mode)
         (elixir-ts-mode . +elixir-mode)))

;;;###autoload
(defun +elixir-mode ()
  (interactive)
  (beginend-prog-mode -1))
