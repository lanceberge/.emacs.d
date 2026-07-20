;;; org-srs-review-extras.el --- Review interface for Org-srs -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org-srs)
(require 'transient)

(defvar +org-srs-review--mode-buffers nil
  "Buffers in which `+org-srs-review-mode' was enabled for a review.")

(defvar +org-srs-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'+org-srs-review-menu)
    map)
  "Keymap for `+org-srs-review-mode'.")

;;;###autoload
(defun +org-srs-review-start ()
  "Start an Org-srs review session."
  (interactive)
  (call-interactively #'org-srs-review-start))

;;;###autoload
(define-minor-mode +org-srs-review-mode
  "Provide local controls while reviewing Org-srs cards."
  :lighter " SRS"
  :keymap +org-srs-review-mode-map
  (if +org-srs-review-mode
      (progn
        (cl-pushnew (current-buffer) +org-srs-review--mode-buffers)
        (add-hook 'org-srs-item-after-confirm-hook #'+org-srs-review--menu-open 90 t)
        (add-hook 'org-srs-review-continue-hook #'+org-srs-review--mode-disable 40 t))
    (setq +org-srs-review--mode-buffers
          (delq (current-buffer) +org-srs-review--mode-buffers))
    (remove-hook 'org-srs-item-after-confirm-hook #'+org-srs-review--menu-open t)
    (remove-hook 'org-srs-review-continue-hook #'+org-srs-review--mode-disable t)))

;;;###autoload
(transient-define-prefix +org-srs-review-menu ()
  "Rate the current Org-srs card."
  [[("1" "Easy" org-srs-review-rate-easy)
    ("2" "Good" org-srs-review-rate-good)
    ("3" "Hard" org-srs-review-rate-hard)
    ("4" "Again" org-srs-review-rate-again)
    ("q" "Quit" org-srs-review-quit)]])

;;;###autoload
(defun +org-srs-review--menu-open (&rest _)
  "Open `+org-srs-review-menu' after revealing an Org-srs card."
  (+org-srs-review-menu))

;;;###autoload
(defun +org-srs-review--mode-enable (&rest _)
  "Enable `+org-srs-review-mode' before reviewing an Org-srs card."
  (+org-srs-review-mode +1))

;;;###autoload
(defun +org-srs-review--mode-disable (&rest _)
  "Disable `+org-srs-review-mode' after reviewing an Org-srs card."
  (+org-srs-review-mode -1))

;;;###autoload
(defun +org-srs-review--mode-disable-all ()
  "Disable `+org-srs-review-mode' in all buffers used by the review."
  (dolist (buffer (copy-sequence +org-srs-review--mode-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (+org-srs-review-mode -1)))))

(add-hook 'org-srs-item-before-review-hook #'+org-srs-review--mode-enable)
(add-hook 'org-srs-review-finish-hook #'+org-srs-review--mode-disable-all)

(provide 'org-srs-review-extras)
;;; org-srs-review-extras.el ends here
