;;; -*- lexical-binding: t -*-
(setq query-replace-skip-read-only t)
(use-package visual-regexp
  :bind
  (:map +normal-mode-map
        ("%" . +replace)))

;;;###autoload
(defun +replace ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (call-interactively #'vr/replace)
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'vr/replace)))))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/command-python
        (replace-regexp-in-string "python " "python3 " vr/command-python)))

;;;###autoload
(defun +range-has-read-only-p (beg end)
  "Non-nil if any position in [BEG, END) has a `read-only' property.
Considers both text properties and overlays."
  (if (= beg end)
      (get-char-property beg 'read-only)
    (let ((pos beg) found)
      (while (and (< pos end) (not found))
        (when (get-char-property pos 'read-only)
          (setq found t))
        (setq pos (next-single-char-property-change pos 'read-only nil end)))
      found)))

;;;###autoload
(defun +vr--filter-read-only-replacements (orig-fn &rest args)
  "Drop matches inside read-only text from `vr--get-replacements' output."
  (let ((result (apply orig-fn args)))
    (list
     (with-current-buffer vr--target-buffer
       (cl-remove-if (lambda (info)
                       (let* ((match-data (cadr info))
                              (beg (cl-first match-data))
                              (end (cl-second match-data)))
                         (+range-has-read-only-p beg end)))
                     (car result)))
     (cadr result))))

;;;###autoload
(defun +vr--skip-read-only-feedback (orig-fn i j beg end)
  "Suppress preview overlays for matches inside read-only text."
  (unless (with-current-buffer vr--target-buffer
            (+range-has-read-only-p beg end))
    (funcall orig-fn i j beg end)))

;; filter read only segments when replacing
(with-eval-after-load 'visual-regexp
  (advice-add 'vr--get-replacements :around #'+vr--filter-read-only-replacements)
  (advice-add 'vr--feedback-match-callback :around #'+vr--skip-read-only-feedback))
