;;; -*- lexical-binding: t -*-
(when IS-MAC
  (use-package tramp ; access remote files within emacs
    :ensure nil
    :ensure (:wait t)))

(when IS-LINUX
  (require 'tramp))
