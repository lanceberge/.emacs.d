;;; -*- lexical-binding: t -*-
(use-package +mark-forward
  :ensure nil
  :bind
  (:repeat-map mark-forward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-forward-sentence )
               ("p" . #'+mark-forward-paragraph)
               ("d" . #'+mark-forward-sexp)
               ("t" . #'+mark-backward-ts-node)
               :exit
               ("g" . (lambda () (interactive))))
  (:map mark-forward-keymap
        ("p" . #'+mark-forward-paragraph)
        ("d" . #'+mark-forward-sexp)
        ("s" . #'+mark-forward-sentence)
        ("t" . #'+mark-forward-ts-node)
        ("b" . #'+mark-forward-buffer)))

(use-package +mark-backward
  :ensure nil
  :bind
  (:repeat-map mark-backward-repeat-map
               ("-" . #'+mark-forward-backward-ring-pop)
               ("." . #'repeat)
               ("s" . #'+mark-backward-sentence )
               ("p" . #'+mark-backward-paragraph)
               ("d" . #'+mark-backward-sexp)
               ("t" . #'+mark-backward-ts-node)
               :exit
               ("g" . (lambda () (interactive))))
  (:map mark-backward-keymap
        ("p" . #'+mark-backward-paragraph)
        ("d" . #'+mark-backward-sexp)
        ("s" . #'+mark-backward-sentence)
        ("b" . #'+mark-backward-buffer)
        ("t" . #'+mark-backward-ts-node)))

(defvar +mark-forward-backward-ring nil
  "ring of (mark . point)")

(defvar +mark-forward-backward-last-forward-command nil)
(defvar +mark-forward-backward-last-backward-command)

;;;###autoload
(defun +mark-forward-backward-ring-pop ()
  (interactive)
  (cond ((null +mark-forward-backward-ring)
         (user-error "there is no history"))
        ((not (region-active-p))
         (let* ((mark-ring-head (car +mark-forward-backward-ring))
                (head-mark (car mark-ring-head))
                (head-point (cdr mark-ring-head)))
           (setq +mark-forward-backward-ring (cdr +mark-forward-backward-ring))
           (goto-char head-point)
           (set-mark head-mark)
           (activate-mark)))
        (t
         (+mark-forward-backward--ring-pop))))

;;;###autoload
(defun +mark-forward-backward--ring-pop ()
  (let* ((mark-ring-head (car +mark-forward-backward-ring))
         (head-mark (car mark-ring-head))
         (head-point (cdr mark-ring-head))
         (head-region-beginning (min head-mark head-point))
         (head-region-end (max head-mark head-point)))
    (if (and (eq (region-beginning) head-region-beginning)
             (eq (region-end) head-region-end))
        (progn
          (setq +mark-forward-backward-ring (cdr +mark-forward-backward-ring))
          (if (null +mark-forward-backward-ring)
              (deactivate-mark)
            (+mark-forward-backward-ring-pop)))
      (cond ((> (point) (mark))
             (goto-char head-region-end)
             (set-mark head-region-beginning))
            (t
             (goto-char head-region-beginning)
             (set-mark head-region-end))))))

;;;###autoload
(defun +mark-forward-backward-ring-push ()
  (cond ((null +mark-forward-backward-ring)
         (add-to-list '+mark-forward-backward-ring
                      (cons (mark) (point))))
        ((region-active-p)
         ;; add the region to the ring if it isn't already there
         (let* ((mark-ring-head (car +mark-forward-backward-ring))
                (head-region-beginning (min (car mark-ring-head) (cdr mark-ring-head)))
                (head-region-end (max (car mark-ring-head) (cdr mark-ring-head))))
           (unless (and (eq (region-beginning) head-region-beginning)
                        (eq (region-end) head-region-end))
             (add-to-list '+mark-forward-backward-ring
                          (cons (mark) (point))))))
        (t
         (setq +mark-forward-backward-ring nil))))

;; TODO write a macro

;;;###autoload
(defun +mark-forward-paragraph ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-end)
        (forward-paragraph))
    (mark-paragraph)
    (exchange-point-and-mark))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-backward-paragraph ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-beginning)
        (backward-paragraph))
    (mark-paragraph))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-forward-sexp ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-end)
        (forward-sexp))
    (mark-sexp)
    (exchange-point-and-mark))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-backward-sexp ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-beginning)
        (backward-sexp))
    (mark-sexp))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-forward-sentence ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-end)
        (forward-sentence))
    (require 'expand-region)
    (er/mark-sentence)
    (exchange-point-and-mark))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-backward-sentence ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-beginning)
        (backward-sentence))
    (require 'expand-region)
    (er/mark-sentence))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-backward-buffer ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (unless (region-active-p)
    (beginning-of-line)
    (set-mark (point))
    (activate-mark))
  (beginning-of-buffer)
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-forward-buffer ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (unless (region-active-p)
    (beginning-of-line)
    (set-mark (point))
    (activate-mark))
  (end-of-buffer)
  (+mark-forward-backward-ring-push))

;; TODO forward-ts-node
;;;###autoload
(defun +mark-forward-ts-node ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-end)
        (when-let ((node (treesit-node-at (point))))
          (goto-char (treesit-node-end (treesit-node-parent node)))))
    (when-let ((node (treesit-node-at (point))))
      (set-mark (point))
      (goto-char (treesit-node-end node))))
  (+mark-forward-backward-ring-push))

;;;###autoload
(defun +mark-backward-ts-node ()
  (interactive)
  (+mark-forward-backward-ring-push)
  (if (region-active-p)
      (progn
        (+move-point-to-region-beginning)
        (when-let ((node (treesit-node-at (point))))
          (goto-char (treesit-node-start (treesit-node-parent node)))))
    (when-let ((node (treesit-node-at (point))))
      (set-mark (treesit-node-end node))
      (goto-char (treesit-node-start node))))
  (+mark-forward-backward-ring-push))
