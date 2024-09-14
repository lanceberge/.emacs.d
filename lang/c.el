;;; -*- lexical-binding: t -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.cu\\'" . c-mode)
  :hook
  (c++-mode . +cpp-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  ;; TODO
  (when IS-MAC
    (add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin"))

  (defun +cpp-mode ()
    (let ((filename (file-name-nondirectory (buffer-file-name))))
      (setq-local dape-configs
                  `((lldb-dap
                     command "lldb-dap" :type "lldb-dap" modes
                     (c-mode c-ts-mode c++-mode c++-ts-mode)
                     ensure dape-ensure-command
                     command-cwd filename
                     :cwd "." :program ,(file-name-base (buffer-file-name)))))

      (my-leader-def
        :keymaps '(c++-mode-map c++-ts-mode-map)
        "ec" (defun +cpp-compile ()
               (interactive)
               (compile (concat "g++-14 " filename " -std=c++20")))))))
