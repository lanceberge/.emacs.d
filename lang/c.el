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
  (dap-lldb-debug-program "/opt/homebrew/opt/llvm/bin/lldb-dap"))

;;;###autoload
(defun +cpp-mode ()
  (let ((file-name-base (file-name-base (file-name-nondirectory (buffer-file-name)))))
    (setq-local dape-configs
                `((lldb-dap
                   command "lldb-dap" :type "lldb-dap" modes
                   (c-mode c-ts-mode c++-mode c++-ts-mode)
                   ensure dape-ensure-command
                   command-cwd ,(file-name-directory (buffer-file-name))
                   :cwd "." :program ,file-name-base)))))

;;;###autoload
(defun +dape-debug-cpp ()
  (interactive)
  (+cpp-compile "-g")
  (call-interactively #'+dape))

;;;###autoload
(defun +cpp-compile (&optional flags)
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (file-name-base (file-name-base file-name)))
    (compile (concat "g++-14 " file-name " -o " file-name-base " -std=c++20 " flags))))
