;;; org-project-test.el --- Tests for org-project -*- lexical-binding: t -*-

(require 'ert)
(require 'org)
(require 'org-project)
(require 'seq)

(ert-deftest +org-project-edit-todo-does-not-duplicate-subtree-on-capture-load ()
  "Editing a TODO should replace the original subtree as capture loads."
  (let* ((contents "* Tasks\n** TODO stuff\n\n- a\n- b\n")
         (file (make-temp-file "org-project-edit" nil ".org" contents))
         capture-buffer)
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* TODO stuff")
          (+org-project-edit-todo file (copy-marker (line-beginning-position) t))
          (setq capture-buffer
                (seq-find
                 (lambda (buffer)
                   (string-prefix-p
                    (format "CAPTURE-%s" (file-name-nondirectory file))
                    (buffer-name buffer)))
                 (buffer-list)))
          (org-with-wide-buffer
           (let ((buffer-text (buffer-substring-no-properties
                               (point-min)
                               (point-max))))
             (should (= 1 (how-many "^\\*\\* TODO stuff" (point-min) (point-max))))
             (should (= 1 (how-many "^- a" (point-min) (point-max))))
             (should (= 1 (how-many "^- b" (point-min) (point-max))))
             (should (string-match-p "\\`\\* Tasks\n\\*\\* TODO stuff\n\n- a\n- b\n+\\'"
                                     buffer-text)))))
      (when (buffer-live-p capture-buffer)
        (with-current-buffer capture-buffer
          (let ((kill-buffer-query-functions nil))
            (ignore-errors
              (org-capture-kill)))))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest +org-project-mark-done-refiles-under-created-date-before-next-heading ()
  "Marking done should create today's Done date and move a later heading under it."
  (let ((file (make-temp-file "org-project-emacs-shape" nil ".org"
                              "* Stuff to start using\n\n- M-x org-narrow-to-subtree\n* Done\n** 6/15/2026\n*** DONE delete-buffer should also go to the last buffer in the current proj\n*** DONE set up abbrev\n\n- when coding, plan out what abbrevs i might want then figure out a setup for it\n\n* Marks, regions\n\n- The M key is probably bad - the easy kill prefix makes sense\n- easy-mark maintaining it's own stuff is annoying\n** TODO [#C] set up latex, port resume to latex\n")))
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* TODO \\[#[A-Z]\\] set up latex")
      (+org-project--move-heading-to-done (copy-marker (line-beginning-position) t))
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\* 6/21/2026\n\\*\\*\\* DONE \\[#[A-Z]\\] set up latex" nil t))
      (goto-char (point-min))
      (re-search-forward "^\\* Marks, regions")
      (let ((marks-end (save-excursion (org-end-of-subtree t t))))
        (should-not (re-search-forward "set up latex" marks-end t))))))

(provide 'org-project-test)
;;; org-project-test.el ends here
