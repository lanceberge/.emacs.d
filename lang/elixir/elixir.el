;;; -*- lexical-binding: t -*-
(defvar elixir-web-mode-map
  (make-sparse-keymap)
  "Keymap for `elixir-web-mode'.")

(define-minor-mode elixir-web-mode
  "Elixir mode for web files."
  :lighter " Web"
  :keymap elixir-web-mode-map)

(defvar +elixir-mode-map (make-sparse-keymap))

(use-package elixir-mode
  :hook
  ((elixir-mode elixir-ts-mode) . +setup-elixir-map)
  :bind
  (:map +elixir-mode-map
        ("r" . #'+elixir-rename-module)))

(defun +setup-elixir-map ()
  "Set up leader key bindings for elixir-mode."
  (define-key +leader-map "i" +elixir-mode-map))

(use-package elixir-ts-mode
  :hook
  ((elixir-ts-mode elixir-mode) . maybe-elixir-web-mode)
  ((elixir-ts-mode elixir-mode) . +elixir-mode)
  ((elixir-ts-mode elixir-mode) . +elixir--maybe-setup-new-file)
  :bind
  (:map elixir-web-mode-map
        (">" . #'+maybe-close-tag)
        ([remap newline] . #'+elixir-newline)
        ([remap +comment-dwim] . #'+elixir-web-mode-comment)))

(add-hook 'find-file-not-found-functions #'+elixir--maybe-setup-new-file t)
