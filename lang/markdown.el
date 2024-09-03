;;; -*- lexical-binding: t -*-
(use-package markdown-mode
  :ensure nil
  :general
  ('normal 'markdown-mode-map
	   "RET" #'markdown-follow-thing-at-point))

(use-package markdown-toc ; create a table of contents
  :general
  ('markdown-mode-map
   :prefix "C-c"
   "t" #'markdown-toc-generate-toc))
