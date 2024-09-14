;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :hook
  (c++-mode . +cpp-mode)
  (c++-ts-mode . +cpp-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun +cpp-mode ()
    (setq-local tab-width 4)

    (when IS-MAC
      (add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin"))

    (setq-local dape-configs
                `((lldb-dap
                   command "lldb-dap" :type "lldb-dap" modes
                   (c-mode c-ts-mode c++-mode c++-ts-mode)
                   ensure dape-ensure-command
                   command-cwd ,(file-name-directory (buffer-file-name))
                   :cwd "." :program ,(file-name-base (buffer-file-name))))))

  :general
  (my-leader-def
    :keymaps '(c++-mode-map c++-ts-mode-map)
    "ec" (defun +cpp-compile ()
           (interactive)
           (compile (concat "g++-14 "
                            (file-name-nondirectory (buffer-file-name)) " -std=c++20")))))
