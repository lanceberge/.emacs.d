;;; org-project-test.el --- Tests for org-project -*- lexical-binding: t -*-

(require 'ert)
(require 'org)
(require 'org-project)

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
