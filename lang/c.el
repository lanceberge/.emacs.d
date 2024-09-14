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
    (let ((file-name-base (file-name-base (file-name-nondirectory (buffer-file-name)))))
      (setq-local dape-configs
                  `((lldb-dap
                     command "lldb-dap" :type "lldb-dap" modes
                     (c-mode c-ts-mode c++-mode c++-ts-mode)
                     ensure dape-ensure-command
                     command-cwd ,(file-name-directory (buffer-file-name))
                     :cwd "." :program ,file-name-base)))))

  :general
  (my-leader-def
    :keymaps '(c++-mode-map c++-ts-mode-map)
    "ec" #'+cpp-compile)

  ;; TODO fix
  (my-localleader-def
    :keymaps '(c++-mode-map c++-ts-mode-map c-ts-mode-map)
    "dd" #'+dape-debug))

;;;###autoload
(defun +dape-debug ()
  (interactive)
  (+cpp-compile "-g")
  (call-interactively #'dape))
;;;###autoload

(defun +cpp-compile (&optional flags)
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (file-name-base (file-name-base file-name)))
    (compile (concat "g++-14 " file-name " -o " file-name-base " -std=c++20 " flags))))
