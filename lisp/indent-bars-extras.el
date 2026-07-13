;;; -*- lexical-binding: t -*-
(require 'treesit)
(require 'indent-bars)

;; Hacks to make indent bars work only in phoenix ~H sigils.
;;;###autoload
(defun +indent-bars--inside-heex-sigil-p (pos)
  "Return non-nil when POS is inside an Elixir ~H HEEx sigil."
  (and (fboundp 'treesit-language-at)
       (eq (treesit-language-at pos) 'heex)
       (let ((node (treesit-node-at pos 'elixir)))
         (while (and node
                     (not (string= (treesit-node-type node) "quoted_content")))
           (setq node (treesit-node-parent node)))
         (when-let* ((name (and node (treesit-node-prev-sibling node t))))
           (and (string= (treesit-node-type name) "sigil_name")
                (string= (treesit-node-text name t) "H"))))))

;;;###autoload
(defun +indent-bars--display-heex-only (beg end &rest args)
  "Draw indent bars only inside Elixir ~H sigils."
  (when (+indent-bars--inside-heex-sigil-p beg)
    (apply #'indent-bars--display beg end args)))

;;;###autoload
(defun +indent-bars--display-heex-blank-lines-only (beg end &rest args)
  "Draw blank-line indent bars only inside Elixir ~H sigils."
  (when (+indent-bars--inside-heex-sigil-p beg)
    (apply #'indent-bars--display-blank-lines beg end args)))

;;;###autoload
(defun +indent-bars-elixir-heex-mode ()
  (require 'indent-bars)
  "Enable `indent-bars-mode' only for HEEx content inside Elixir ~H sigils."
  (setq-local indent-bars-no-descend-string nil
              indent-bars-no-descend-lists nil
              indent-bars-spacing-override 2
              indent-bars--display-blank-lines-function #'+indent-bars--display-heex-blank-lines-only)
  indent-bars--display-function #'+indent-bars--display-heex-only
  (indent-bars-mode 1))

(provide 'indent-bars-extras)
