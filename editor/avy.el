;;; -*- lexical-binding: t -*-
(use-package avy
  :after isearch
  :custom
  (avy-keys '(?j ?d  ?s ?l ?a ?g ?h ?e ?i ?c ?n))
  (avy-single-candidate-jump nil)
  (avy-case-fold-search nil)
  :bind
  ("C-;" . #'avy-goto-char-2)
  (:map isearch-mode-map
        ("C-;" . #'+avy-isearch))
  :config
  (setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-char-2-below . avy-order-closest)
                           (avy-goto-char-2-above . avy-order-closest))))

;;;###autoload
(defun +avy-isearch ()
  (interactive)
  (let ((avy-single-candidate-jump t))
    (avy-isearch)))

(use-package avy-actions
  :ensure (:type file :main "~/.emacs.d/lisp/avy-actions.el")
  :after avy
  :demand t
  :config
  (setq avy-dispatch-alist
        (list
         (cons ?\s 'avy-action-mark-until-pt)
         (cons ?w 'avy-action-kill-to-point)
         (cons ?, 'avy-action-embark)
         (cons ?' 'avy-action-embark-dwim)

         (cons ?p 'avy-action-yank-move)

         (cons ?k 'avy-action-kill-line-stay)
         (cons ?K 'avy-action-kill-line-move)

         (cons ?t 'avy-action-move-line-or-region-stay)
         (cons ?T 'avy-action-move-line-or-region-move)

         (cons ?x 'avy-action-kill-whole-lines))))
