;;; -*- lexical-binding: t -*-

(use-package scala-mode
  :disabled t
  :interpreter ("scala" . scala-mode)
  :config
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk-amd64"))

(use-package sbt-mode
  :disabled t
  :commands sbt-start sbt-command
  :config
  ;; https://github.com/ensime/emacs-sbt-mode/issues/31
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package winner ; Undo and redo window configs
  :disabled t ;; using tab bar history instead
  :ensure nil
  :hook
  (emacs-startup . winner-mode)
  :bind
  (:repeat-map winner-repeat-map
               ("]" . #'winner-redo)
               ("[" . #'winner-undo)))

(use-package ace-link
  :bind
  (:map text-mode-map
        ("M-i" . #'ace-link))
  (:map helpful-mode-map
        ("M-i" . #'ace-link)))

(use-package beginend
  :disabled t
  :bind
  (:map +leader-map
        ("[" . #'beginning-of-buffer)
        ("]" . #'end-of-buffer))
  :hook (after-init . beginend-global-mode))

;; (bind-key "[" mark-backward-keymap +normal-mode-map)
;; (bind-key "]" mark-forward-keymap +normal-mode-map)

(use-package +mark-forward-backward
  :disabled t ;; using easy-kill instead
  :ensure (:type file :main "~/.emacs.d/lisp/mark-forward-backward.el")
  :bind
  (:repeat-map mark-backward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-backward-sentence )
               ("p" . #'+mark-backward-paragraph)
               ("w" . #'+mark-backward-word)
               ("d" . #'+mark-backward-sexp)
               :exit
               ("g" . (lambda () (interactive))))
  (:map +normal-mode-map
        ("B" . #'+mark-backward-word))

  (:map mark-backward-keymap
        ("p" . #'+mark-backward-paragraph)
        ("d" . #'+mark-backward-sexp)
        ("s" . #'+mark-backward-sentence)
        ("w" . #'+mark-backward-word)))

(use-package +mark-backward
  :disabled t
  :ensure nil
  :bind
  (:repeat-map mark-forward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-forward-sentence )
               ("p" . #'+mark-forward-paragraph)
               ("w" . #'+mark-forward-word)
               ("d" . #'+mark-forward-sexp)
               :exit
               ("g" . (lambda () (interactive))))
  (:map mark-forward-keymap
        ("p" . #'+mark-forward-paragraph)
        ("w" . #'+mark-forward-word)
        ("d" . #'+mark-forward-sexp)
        ("s" . #'+mark-forward-sentence)))
