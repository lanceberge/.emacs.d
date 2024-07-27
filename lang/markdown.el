;;; -*- lexical-binding: t -*-
(use-package markdown-mode
  :general
  ('normal markdown-mode-map "RET" #'markdown-follow-thing-at-point))

(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode)
  :general
  ('markdown-mode-map
   :prefix "C-c"
   "e" #'(pandoc-main-hydra/body :which-key "pandoc")))

(use-package markdown-toc ; create a table of contents
  :general
  ('markdown-mode-map
   :prefix "C-c"
   "t" #'markdown-toc-generate-toc))
