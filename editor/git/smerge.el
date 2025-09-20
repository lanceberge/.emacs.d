(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode)
  :bind
  (:map smerge-basic-map
        ("n" . #'+smerge-vc-next-conflict))
  (:map +leader-map
        ("RET" . #'smerge-keep-current)
        ("mu" . #'smerge-keep-upper)
        ("mo" . #'smerge-keep-lower)
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
     (vc-find-conflicted-file)
     (unless (looking-at "^<<<<<<<")
       (let ((prev-pos (point)))
         (goto-char (point-min))
         (unless (ignore-errors (not (smerge-next)))
           (goto-char prev-pos)))))))
