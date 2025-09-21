(use-package git-scroll
  :ensure nil
  :bind
  (:repeat-map git-scroll-repeat-map
               ("n" . #'+next-hunk-or-file)
               ("p" . #'+previous-hunk-or-file))
  (:map +leader-map
        ("gn" . #'+next-hunk-or-file)
        ("gp" . #'+previous-hunk-or-file)))

;;;###autoload
(defun +modified-git-files ()
  (when (vc-git-root default-directory)
    (let ((unstaged (split-string
                     (shell-command-to-string
                      "git diff --name-only")
                     "\n" t))
          (staged (split-string
                   (shell-command-to-string
                    "git diff --cached --name-only")
                   "\n" t)))
      (append unstaged staged))))

;;;###autoload
(defun +next-modified-git-file ()
  "Go to the next modified git file."
  (interactive)
  (let* ((modified-files (+modified-git-files))
         (current-file (file-relative-name buffer-file-name (vc-git-root default-directory)))
         (current-index (seq-position modified-files current-file #'string=))
         (next-file (if current-index
                        (nth (mod (1+ current-index) (length modified-files)) modified-files)
                      (car modified-files))))
    (if next-file
        (find-file (expand-file-name next-file (vc-git-root default-directory)))
      (user-error "No modified git files found"))))

;;;###autoload
(defun +previous-modified-git-file ()
  "Go to the previous modified git file."
  (interactive)
  (let* ((modified-files (+modified-git-files))
         (current-file (file-relative-name buffer-file-name (vc-git-root default-directory)))
         (current-index (seq-position modified-files current-file #'string=))
         (prev-file (if current-index
                        (nth (mod (1- current-index) (length modified-files)) modified-files)
                      (car (last modified-files)))))
    (if prev-file
        (find-file (expand-file-name prev-file (vc-git-root default-directory)))
      (user-error "No modified git files found"))))

;;;###autoload
(defun +next-hunk-or-file ()
  "Go to next hunk in current file, or next modified file if no more hunks."
  (interactive)
  (condition-case nil
      (diff-hl-next-hunk)
    (user-error
     (+next-modified-git-file)
     (condition-case nil
         (progn
           (goto-char (point-min))
           (diff-hl-next-hunk))
       (user-error nil)))))

;;;###autoload
(defun +previous-hunk-or-file ()
  "Go to previous hunk in current file, or previous modified file if no more hunks."
  (interactive)
  (condition-case nil
      (diff-hl-next-hunk t)
    (user-error
     (+previous-modified-git-file)

     (condition-case nil
         (progn
           (goto-char (point-max))
           (diff-hl-next-hunk t))
       (user-error nil)))))
