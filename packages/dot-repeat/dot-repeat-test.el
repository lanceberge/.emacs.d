;;; dot-repeat-test.el --- Tests for dot-repeat -*- lexical-binding: t -*-

(require 'ert)
(require 'modal)
(require '+dot-repeat)

(defmacro +dot-repeat-test-with-buffer (&rest body)
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer " *dot-repeat-test*"))
         (+dot-repeat-excluded-commands nil))
     (unwind-protect
         (progn
           (switch-to-buffer buffer)
           (text-mode)
           (visual-line-mode 1)
           (+dot-repeat-mode 1)
           (+normal-mode 1)
           (setq mark-active nil)
           (deactivate-mark)
           (setq +dot-repeat--last-repeat nil
                 +dot-repeat--pending-insert-episode nil
                 +dot-repeat--suppress-current-command nil
                 +dot-repeat--last-command-history (car command-history))
           ,@body)
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(ert-deftest +dot-repeat-replays-insert-episode ()
  (+dot-repeat-test-with-buffer
    (execute-kbd-macro "i")
    (execute-kbd-macro "test")
    (execute-kbd-macro (kbd "<escape>"))
    (+dot-repeat)
    (should (equal (buffer-string) "testtest"))
    (should +normal-mode)
    (should-not +insert-mode)))

(ert-deftest +dot-repeat-replayed-insert-episode-returns-to-normal ()
  (+dot-repeat-test-with-buffer
    (setq +dot-repeat--last-repeat
          (list :type 'insert-episode
                :entry-command '+insert-mode
                :entry-prefix nil
                :body-macro "test"))
    (+dot-repeat)
    (should (equal (buffer-string) "test"))
    (should +normal-mode)
    (should-not +insert-mode)))

(ert-deftest +dot-repeat-replays-entry-edit-with-exit ()
  (+dot-repeat-test-with-buffer
    (insert "abcdef")
    (goto-char (point-min))
    (execute-kbd-macro (kbd "C-k"))
    (execute-kbd-macro (kbd "<escape>"))
    (insert "abcdef")
    (goto-char (point-min))
    (+dot-repeat)
    (should (equal (buffer-string) ""))
    (should +normal-mode)
    (should-not +insert-mode)))

(ert-deftest +dot-repeat-replays-entry-edit-with-insert-body ()
  (+dot-repeat-test-with-buffer
    (insert "abcdef")
    (goto-char (point-min))
    (execute-kbd-macro (kbd "C-k"))
    (execute-kbd-macro "test")
    (execute-kbd-macro (kbd "<escape>"))
    (insert "abcdef")
    (goto-char (point-min))
    (+dot-repeat)
    (should (equal (buffer-string) "test"))
    (should +normal-mode)
    (should-not +insert-mode)))

(ert-deftest +dot-repeat-replays-entry-edit-with-read-char-argument ()
  (+dot-repeat-test-with-buffer
    (insert "aaXbbXcc")
    (goto-char (point-min))
    (execute-kbd-macro (kbd "M-z X"))
    (execute-kbd-macro (kbd "<escape>"))
    (forward-char 1)
    (+dot-repeat)
    (should (equal (buffer-string) "XXcc"))
    (should +normal-mode)
    (should-not +insert-mode)))

(ert-deftest +dot-repeat-replays-normal-edit ()
  (+dot-repeat-test-with-buffer
    (insert "abc")
    (goto-char (point-min))
    (execute-kbd-macro "d")
    (goto-char (point-min))
    (+dot-repeat)
    (should (equal (buffer-string) "c"))))

(ert-deftest +dot-repeat-replays-normal-edit-consecutively ()
  (+dot-repeat-test-with-buffer
    (insert "abc")
    (goto-char (point-min))
    (execute-kbd-macro "d")
    (let ((repeat +dot-repeat--last-repeat))
      (goto-char (point-min))
      (execute-kbd-macro ".")
      (goto-char (point-min))
      (execute-kbd-macro ".")
      (should (equal (buffer-string) ""))
      (should (equal +dot-repeat--last-repeat repeat)))))

(ert-deftest +dot-repeat-replays-insert-episode-consecutively ()
  (+dot-repeat-test-with-buffer
    (execute-kbd-macro "i")
    (execute-kbd-macro "test")
    (execute-kbd-macro (kbd "<escape>"))
    (let ((repeat +dot-repeat--last-repeat))
      (execute-kbd-macro ".")
      (execute-kbd-macro ".")
      (should (equal (buffer-string) "testtesttest"))
      (should +normal-mode)
      (should-not +insert-mode)
      (should (equal +dot-repeat--last-repeat repeat)))))

(ert-deftest +dot-repeat-empty-insert-does-not-replace-previous-repeat ()
  (+dot-repeat-test-with-buffer
    (insert "abc")
    (goto-char (point-min))
    (execute-kbd-macro "d")
    (execute-kbd-macro "i")
    (execute-kbd-macro (kbd "<escape>"))
    (goto-char (point-min))
    (+dot-repeat)
    (should (equal (buffer-string) "c"))))

(ert-deftest +dot-repeat-errors-outside-normal-mode ()
  (+dot-repeat-test-with-buffer
    (execute-kbd-macro "i")
    (should-error (+dot-repeat) :type 'user-error)))

(provide 'dot-repeat-test)
;;; dot-repeat-test.el ends here
