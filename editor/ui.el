;;; -*- lexical-binding: t -*-
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (find-file . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package paren ; show matching parentheses
  :ensure nil
  :hook ((prog-mode text-mode) . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package hydra
  :defer t)
