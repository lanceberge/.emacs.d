;;; -*- lexical-binding: t -*-
(use-package window
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map ctl-x-map
        ("6" . #'+new-window-prefix)))

(use-package +other-window-scroll-repeat
  :ensure nil
  :bind
  (:repeat-map +window-scroll-repeat-map
               ("M-v" . #'scroll-other-window-down)
               ("C-v" . #'scroll-other-window)
               ("v" . #'scroll-other-window)
               ("." . #'repeat))
  (:map ctl-x-map
        ("w M-v" . #'scroll-other-window-down)
        ("w C-v" . #'scroll-other-window)
        ("wv" . #'scroll-other-window)))

(use-package +window
  :ensure nil
  :bind
  (:map +leader-map
        ("SPC l" . #'+other-buffer)))

(use-package other-window
  :ensure nil
  :bind
  ("M-o" . #'other-window)
  ("M-O" . #'+other-window-previous))

(advice-add 'other-window :before
            (defun other-window-split-if-single (&rest _)
              "Split the frame if there is a single window."
              (when (one-window-p) (split-window-sensibly))))

;;;###autoload
(defun +other-window-previous ()
  (interactive)
  (if (one-window-p) (split-window-sensibly)
    (setq repeat-map 'other-window-repeat-map)
    (other-window -1)))

(use-package ace-window
  :hook
  (emacs-startup . ace-window-display-mode)
  :custom
  (aw-dispatch-when-more-than 1)
  (aw-dispatch-alist
   '((?k aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?j aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?e aw-execute-command-other-window "Execute Command Other Window")
     (?F aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?T aw-transpose-frame "Transpose Frame")
     (?? aw-show-dispatch-help)))
  :bind
  (:map +leader-map
        ("fw" . #'ace-window)))

;;;###autoload
(defun +ace-window-find-file (file)
  "Open FILE in an ace-selected window."
  (interactive "FFind file: ")
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+ace-window--ensure-dispatch-window)
    (select-window (aw-select " Ace - Find File"))
    (find-file file)))

;;;###autoload
(defun +ace-window-switch-to-buffer (buffer)
  "Open BUFFER in an ace-selected window."
  (interactive "BSwitch to buffer: ")
  (require 'ace-window)
  (let ((aw-dispatch-when-more-than 2))
    (+ace-window--ensure-dispatch-window)
    (select-window (aw-select " Ace - Switch Buffer"))
    (switch-to-buffer buffer)))

;;;###autoload
(defun +find-file-new-window (file)
  "Open FILE in a newly-created window."
  (interactive "FFind file: ")
  (set-window-buffer (+window-split-new) (find-file-noselect file)))

;;;###autoload
(defun +switch-to-buffer-new-window (buffer)
  "Open BUFFER in a newly-created window."
  (interactive "BSwitch to buffer: ")
  (set-window-buffer (+window-split-new) (get-buffer-create buffer)))

;;;###autoload
(defun +ace-window--ensure-dispatch-window ()
  "Create a second window when ace-window would have no target choice."
  (when (= (length (window-list nil 'no-minibuf)) 1)
    (+window-split-new)))

;;;###autoload
(defun +window-split-new (&optional window)
  "Split WINDOW and return the new window.
WINDOW defaults to the active minibuffer's originating window, or
the selected window when no minibuffer is active."
  (or (seq-some #'+window-split-window
                (delq nil (delete-dups
                           (append
                            (list window
                                  (minibuffer-selected-window)
                                  (get-mru-window nil nil nil t)
                                  (get-largest-window nil nil nil t)
                                  (selected-window))
                            (window-list nil 'no-minibuf)))))
      (user-error "Cannot split window")))

;;;###autoload
(defun +window-split-window (window)
  "Split WINDOW and return the new window, or nil if it cannot be split."
  (unless (window-minibuffer-p window)
    (or (split-window-sensibly window)
        (let* ((width (window-total-width window))
               (height (window-total-height window))
               (prefer-side-by-side (> width (* height 2)))
               (preferred (if prefer-side-by-side
                              #'split-window-right
                            #'split-window-below))
               (fallback (if prefer-side-by-side
                             #'split-window-below
                           #'split-window-right)))
          (or (funcall preferred nil window)
              (funcall fallback nil window))))))

;;;###autoload
(defun +new-window-prefix ()
  "Display the buffer of the next command in a newly-created window."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_buffer _alist)
     (cons (+window-split-new) 'window))
   nil "[new-window]")
  (message "Display next command buffer in a new window..."))

(use-package frame
  :ensure nil
  :bind
  ("s-n" . #'make-frame)
  (:map +leader2-map
        ("fo" . #'other-frame)
        ("fn" . #'make-frame)
        ("ff" . #'select-frame-by-name)
        ("fd" . #'delete-frame)))

;; switch to the "other window" which is the last window you spent more than `switchy-window-delay' on
(use-package switchy-window
  :hook
  (emacs-startup . switchy-window-minor-mode)
  :bind
  (:map ctl-x-map
        ("wj" . #'switchy-window)))

;;;###autoload
(defun +switchy-window-other-window ()
  "Shitty hack to return the window `switchy-window' would switch to assuming the delay had passed."
  (require 'switchy-window)
  (let ((switchy-window--tick-counter switchy-window--tick-counter)
        (switchy-window--tick-alist (copy-tree switchy-window--tick-alist))
        (switchy-window--visited-windows switchy-window--visited-windows)
        (last-command nil)
        target)
    (let ((window-selection-change-functions
           (remove #'switchy-window--on-window-selection-change
                   window-selection-change-functions)))
      (save-window-excursion
        (switchy-window)
        (setq target (selected-window))))
    target))

(setq other-window-scroll-default #'+switchy-window-other-window)

(use-package winner ; Undo and redo window configs
  :ensure nil
  :hook
  (emacs-startup . winner-mode)
  :bind
  (:repeat-map winner-repeat-map
               ("p" . #'winner-undo)
               ("n" . #'winner-redo)))

(use-package windresize
  :custom
  (windresize-default-increment 3)
  :bind
  (:map ctl-x-map
        ("wr" . #'windresize))
  (:map windresize-map
        ("h" . #'windresize-left)
        ("l" . #'windresize-right)
        ("p" . #'windresize-up)
        ("n" . #'windresize-down)
        ("g" . #'windresize-exit)))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-name-function #'+tab-bar-tab-name-project)
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :bind
  (:map +normal-mode-map
        ("]t" . #'tab-bar-switch-to-next-tab)
        ("[t" . #'tab-bar-switch-to-prev-tab))
  (:map ctl-x-map
        ("to" . #'tab-bar-switch-to-recent-tab)
        ("t[" . #'tab-bar-switch-to-prev-tab)
        ("t]" . #'tab-bar-switch-to-next-tab)
        ("tf" . #'+consult-tab)
        ("1" . #'+toggle-tab-zoom))
  (:map +leader-map
        ("SPC t" . #'+other-tab-switch-project)
        ("nt" . #'tab-bar-new-tab)))

;;;###autoload
(defun +other-tab-switch-project ()
  (interactive)
  (other-tab-prefix)
  (+project-switch-project-buffer))

;;;###autoload
(defun +tab-bar-tab-name-project ()
  "Generate tab name as PROJECT:BUFFER from the selected window's buffer."
  (let* ((win (or (minibuffer-selected-window)
                  (and (window-minibuffer-p) (get-mru-window))))
         (buf (window-buffer win))
         (name (buffer-name buf))
         (project (with-current-buffer buf (project-current))))
    (if project
        (format "%s:%s"
                (file-name-nondirectory
                 (directory-file-name (project-root project)))
                name)
      name)))

;;;###autoload
(defun +toggle-tab-zoom ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and tab-bar-mode
           (equal (selected-window) (next-window)))
      (tab-bar-history-back)
    ;; Force tab-bar history to record this command even if it was repeated.
    (setq tab-bar-history-done-command nil)
    (delete-other-windows)))

(use-package consult-tab-bar
  :ensure (:type file :main "~/.emacs.d/lisp/consult-tab.el")
  :bind
  (:map ctl-x-map
        ("tf" . #'+consult-tab)))

(use-package tab-bar-repeat
  :ensure nil
  :bind
  (:repeat-map window-repeat-map
               ("p" . #'tab-bar-history-back)
               ("n" . #'tab-bar-history-forward))
  (:map ctl-x-map
        ("wp" . #'tab-bar-history-back)
        ("wn" . #'tab-bar-history-forward)))

;;;###autoload
(defun +revert-buffer ()
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun +other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(use-package pulsar
  :hook
  (window-buffer-change-functions . +pulsar-pulse-line-no-minibuffer))

(defvar +pulsar--last-buffer nil
  "Buffer in the selected window the last time we pulsed.")

;;;###autoload
(defun +pulsar-pulse-line-no-minibuffer (frame-or-window)
  (let* ((win (if (framep frame-or-window)
                  (frame-selected-window frame-or-window)
                frame-or-window))
         (buf (window-buffer win)))
    (unless (or (minibufferp buf)
                (eq buf +pulsar--last-buffer))
      (setq +pulsar--last-buffer buf)
      (with-selected-window win
        (pulsar-pulse-line)))))
