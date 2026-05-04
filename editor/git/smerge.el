;;; -*- lexical-binding: t -*-
(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode)
  :bind
  (:map smerge-basic-map
        ("n" . #'+smerge-vc-next-conflict))
  (:map +leader-map
        ("mu" . #'smerge-keep-upper)
        ("ml" . #'smerge-keep-lower)
        ("ma" . #'smerge-keep-all)
        ("mm" . #'smerge-ediff)
        ("mn" . #'+smerge-vc-next-conflict)
        ("mp" . #'smerge-prev))
  :config
  (dolist (func '(smerge-keep-current smerge-keep-upper smerge-keep-lower smerge-keep-all))
    (advice-add func :after #'+save-buffer-advice))

  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

;;;###autoload
(defun +save-buffer-advice ()
  (when (buffer-modified-p)
    (save-buffer)))

;;;###autoload
(defun +smerge-vc-next-conflict ()
  (interactive)
  (require 'vc)
  (condition-case nil
      (smerge-next)
    (error
     (if (and (buffer-modified-p) buffer-file-name)
         (save-buffer))
     (let* ((root (vc-call-backend 'Git 'root default-directory))
            (files (vc-call-backend 'Git 'conflicted-files
                                    (or root default-directory))))
       (when (equal (car files) buffer-file-name) (pop files))
       (if (null files)
           (message "No more conflicted files")
         (find-file (pop files))
         (message "%s more conflicted files after this one"
                  (if files (length files) "No"))))
     (unless (looking-at "^<<<<<<<")
       (let ((prev-pos (point)))
         (goto-char (point-min)))))))
