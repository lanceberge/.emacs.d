;;; -*- lexical-binding: t -*-
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t))

(use-package csv-mode
  :ensure nil)

(use-package json-mode
  :ensure nil)

(use-package clojure-mode)

(use-package cider)

(use-package go-mode
  :config
  (setq-local use-tabs-mode t))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-basic-offset 2)
  :hook (js-mode . (lambda () (setq-local tab-width 2))))

(use-package markdown-mode
  :ensure nil
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package text-mode
  :ensure nil
  :custom
  (text-mode-ispell-word-completion nil))

(use-package yaml-mode
  :ensure nil
  :mode ("\\.gotmpl\\'" . yaml-mode)
  :hook
  (yaml-mode
   .
   (lambda ()
     (if (string-suffix-p ".yaml.gotmpl" buffer-file-name)
         (remove-hook 'before-save-hook #'whitespace-cleanup)))))

(use-package dockerfile-mode)

(use-package terraform-mode)

(use-package sh-mode
  :ensure nil
  :hook
  (sh-base-mode
   .
   ;; make scripts executable on save
   (lambda ()
     (add-hook 'after-save-hook
               'executable-make-buffer-file-executable-if-script-p
               nil
               t))))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :config
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk-amd64"))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; https://github.com/ensime/emacs-sbt-mode/issues/31
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package nxml-mode
  :ensure nil
  :bind
  (:map nxml-mode-map
        ([remap save-buffer] .
         (lambda () (interactive)
           (undo-tree-mode -1)
           (save-buffer)
           (undo-tree-mode))))
  :custom
  (nxml-auto-insert-xml-declaration-flag nil)
  (nxml-validate-on-save nil))

(use-package rust-mode)

(use-package nix-mode)
