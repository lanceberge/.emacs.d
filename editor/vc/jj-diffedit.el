;;; jj-diffedit.el --- Interactive diff editor for jj split/squash -*- lexical-binding: t -*-

;; Author: Lance Bergeron
;; Keywords: vc, jj, jujutsu

;;; Commentary:

;; A diff editor that plugs into jj's external-tool protocol (jj split -i,
;; jj squash -i, jj diffedit). jj invokes the tool with two directories,
;; $left (parent, read-only) and $right (current, writable); the tool
;; mutates $right and exits. This package presents the changes one hunk
;; at a time and lets you toggle each one with a single keystroke.
;;
;; Configure jj (in ~/.config/jj/config.toml):
;;
;; [ui]
;; diff-editor = "emacs"
;;
;; [merge-tools.emacs]
;; program = "emacsclient"
;; edit-args = ["-c", "-F", "((name . \"jj-diffedit\"))",
;; "-e", "(jj-diffedit \"$left\" \"$right\")"]
;;
;; Keys (in the diffedit buffer):
;; l keep this hunk (default — included in the split commit)
;; u revert this hunk (excluded from the split commit)
;; n next hunk
;; p previous hunk
;; e drop into ediff for this file
;; g overview of all hunks
;; C-c C-c save and exit (jj-diffedit-save)
;; C-c C-k abort (jj-diffedit-abort)
;; ? help

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'ediff)

;;;; Customization

(defgroup jj-diffedit nil
  "Interactive diff editor for jujutsu."
  :group 'vc
  :prefix "jj-diffedit-")

(defcustom jj-diffedit-diff-program "diff"
  "GNU diff binary used to compute hunks between $left and $right."
  :type 'string)

(defcustom jj-diffedit-patch-program "patch"
  "GNU patch binary used to apply reverse hunks during save."
  :type 'string)

(defcustom jj-diffedit-abort-sentinel ".jj-diffedit-aborted"
  "Filename written into $right when the session is aborted.
A shell wrapper can check for this file and exit non-zero so jj sees
the operation as cancelled."
  :type 'string)

(defface jj-diffedit-header-face
  '((t :inherit mode-line-emphasis :weight bold))
  "Face for the status header.")

(defface jj-diffedit-keep-face
  '((t :inherit success :weight bold))
  "Face for the KEEP decision label.")

(defface jj-diffedit-revert-face
  '((t :inherit warning :weight bold))
  "Face for the REVERT decision label.")

(defface jj-diffedit-ediff-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the EDIFF-RESOLVED decision label.")

;;;; Data model

(cl-defstruct (jj-diffedit-hunk (:constructor jj-diffedit-hunk--make))
  "One hunk inside a file.
DECISION is one of `keep' (default; this change is in the split commit)
or `revert' (this change is rolled back in $right and ends up in the
remainder commit). When the parent file is ediff-resolved, the per-hunk
decisions are ignored."
  file ;; jj-diffedit-file struct
  index ;; index within file (0-based, display only)
  header ;; "@@ -L,N +L',N' @@ ..." line
  text ;; full hunk text incl. header line, newline-terminated
  decision) ;; `keep' or `revert'

(cl-defstruct (jj-diffedit-file (:constructor jj-diffedit-file--make))
  "One changed path between $left and $right.
KIND is one of `modified' (text diff), `added' (only in right),
`deleted' (only in left), `binary' (treated as whole-file toggle), or
`ediff' (set after the user resolves via `e'). When KIND is `ediff'
the per-hunk decisions in HUNKS are ignored on save."
  path ;; relative path string
  left-path ;; absolute path in $left, or nil
  right-path ;; absolute path in $right, or nil
  kind ;; `modified' | `added' | `deleted' | `binary' | `ediff'
  hunks) ;; list of jj-diffedit-hunk

(cl-defstruct (jj-diffedit-session (:constructor jj-diffedit-session--make))
  "All state for one invocation."
  left-dir
  right-dir
  files ;; list of jj-diffedit-file
  flat-hunks ;; vector of jj-diffedit-hunk for navigation
  current ;; index into flat-hunks
  instructions ;; contents of JJ-INSTRUCTIONS or nil
  frame ;; the emacsclient/standalone frame, for clean exit
  buffer) ;; the diffedit buffer

;;;; Buffer-local state

(defvar-local jj-diffedit--session nil
  "Current `jj-diffedit-session' for this buffer.")

;;;; Tree walking and hunk extraction

(defun jj-diffedit--walk-files (dir)
  "Return a sorted list of file paths under DIR (relative, no leading /).
Excludes JJ-INSTRUCTIONS and the abort sentinel."
  (let ((root (file-name-as-directory (expand-file-name dir)))
        files)
    (cl-labels ((walk (subdir)
                  (dolist (entry (directory-files subdir t "\\`[^.]" t))
                    (cond
                     ((file-symlink-p entry) nil)
                     ((file-directory-p entry) (walk entry))
                     (t (push entry files))))))
      (when (file-directory-p root)
        (walk root)))
    (let ((prefix (length root)))
      (sort (mapcar (lambda (f) (substring f prefix)) files)
            #'string<))))

(defun jj-diffedit--binary-file-p (path)
  "Return non-nil if PATH contains a NUL byte in its first 8 KiB."
  (and (file-regular-p path)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally path nil 0 8192)
         (goto-char (point-min))
         (search-forward "\0" nil t))))

(defun jj-diffedit--files-equal-p (a b)
  "Return non-nil if files A and B have identical contents."
  (and (= (file-attribute-size (file-attributes a))
          (file-attribute-size (file-attributes b)))
       (zerop (call-process jj-diffedit-diff-program nil nil nil
                            "-q" "--" a b))))

(defun jj-diffedit--compute-diff (left right)
  "Return the unified-diff output between LEFT and RIGHT files as a string.
Returns nil if the files are identical."
  (with-temp-buffer
    (let ((exit (call-process jj-diffedit-diff-program nil t nil
                              "-u"
                              "--label" "a"
                              "--label" "b"
                              "--" left right)))
      ;; diff exits 0 if same, 1 if different, >=2 on error
      (cond
       ((zerop exit) nil)
       ((= exit 1) (buffer-string))
       (t (error "diff failed (exit %s) between %s and %s" exit left right))))))

(defun jj-diffedit--parse-hunks (diff-text file)
  "Parse DIFF-TEXT, returning a list of `jj-diffedit-hunk' for FILE."
  (let ((hunks nil)
        (i 0))
    (with-temp-buffer
      (insert diff-text)
      (goto-char (point-min))
      ;; skip past the --- / +++ header lines to the first @@
      (when (re-search-forward "^@@ " nil t)
        (beginning-of-line)
        (while (looking-at "^@@ ")
          (let* ((header-start (point))
                 (header-end (line-end-position))
                 (header (buffer-substring-no-properties header-start header-end))
                 (body-start (1+ header-end))
                 (body-end (save-excursion
                             (forward-line 1)
                             (if (re-search-forward "^@@ " nil t)
                                 (line-beginning-position)
                               (point-max)))))
            (push (jj-diffedit-hunk--make
                   :file file
                   :index i
                   :header header
                   :text (buffer-substring-no-properties header-start body-end)
                   :decision 'keep)
                  hunks)
            (cl-incf i)
            (goto-char body-end)))))
    (nreverse hunks)))

(defun jj-diffedit--classify-path (left-dir right-dir path)
  "Build a `jj-diffedit-file' for PATH relative to LEFT-DIR and RIGHT-DIR.
Returns nil if the file is identical on both sides."
  (let* ((lp (expand-file-name path left-dir))
         (rp (expand-file-name path right-dir))
         (l-exists (file-regular-p lp))
         (r-exists (file-regular-p rp)))
    (cond
     ((and l-exists r-exists
           (jj-diffedit--files-equal-p lp rp))
      nil)
     ((and l-exists r-exists
           (or (jj-diffedit--binary-file-p lp)
               (jj-diffedit--binary-file-p rp)))
      (let ((file (jj-diffedit-file--make
                   :path path :left-path lp :right-path rp :kind 'binary)))
        (setf (jj-diffedit-file-hunks file)
              (list (jj-diffedit-hunk--make
                     :file file :index 0
                     :header (format "binary file %s" path)
                     :text (format "binary file %s (no textual diff)\n" path)
                     :decision 'keep)))
        file))
     ((and l-exists r-exists)
      (let* ((file (jj-diffedit-file--make
                    :path path :left-path lp :right-path rp :kind 'modified))
             (diff (jj-diffedit--compute-diff lp rp))
             (hunks (and diff (jj-diffedit--parse-hunks diff file))))
        (when hunks
          (setf (jj-diffedit-file-hunks file) hunks)
          file)))
     (r-exists
      (let ((file (jj-diffedit-file--make
                   :path path :left-path nil :right-path rp :kind 'added)))
        (setf (jj-diffedit-file-hunks file)
              (list (jj-diffedit-hunk--make
                     :file file :index 0
                     :header (format "new file %s" path)
                     :text (jj-diffedit--whole-file-pseudo-hunk rp 'added)
                     :decision 'keep)))
        file))
     (l-exists
      (let ((file (jj-diffedit-file--make
                   :path path :left-path lp :right-path nil :kind 'deleted)))
        (setf (jj-diffedit-file-hunks file)
              (list (jj-diffedit-hunk--make
                     :file file :index 0
                     :header (format "deleted file %s" path)
                     :text (jj-diffedit--whole-file-pseudo-hunk lp 'deleted)
                     :decision 'keep)))
        file)))))

(defun jj-diffedit--whole-file-pseudo-hunk (path kind)
  "Build a synthetic hunk text describing the whole contents of PATH.
KIND is `added' (prefix `+') or `deleted' (prefix `-')."
  (with-temp-buffer
    (if (jj-diffedit--binary-file-p path)
        (insert (format "(binary file, %d bytes)\n"
                        (file-attribute-size (file-attributes path))))
      (insert-file-contents path)
      (let ((prefix (if (eq kind 'added) "+" "-")))
        (goto-char (point-min))
        (while (not (eobp))
          (insert prefix)
          (forward-line 1))))
    (buffer-string)))

;;;; Session construction

(defun jj-diffedit--build-session (left right)
  "Build a `jj-diffedit-session' from LEFT and RIGHT directories."
  (let* ((left (expand-file-name left))
         (right (expand-file-name right))
         (left-files (jj-diffedit--walk-files left))
         (right-files (jj-diffedit--walk-files right))
         (all-paths (cl-remove-duplicates
                     (cl-remove-if (lambda (p)
                                     (or (string= p "JJ-INSTRUCTIONS")
                                         (string= p jj-diffedit-abort-sentinel)))
                                   (append left-files right-files))
                     :test #'string=))
         (sorted (sort all-paths #'string<))
         (files (delq nil
                      (mapcar (lambda (p) (jj-diffedit--classify-path left right p))
                              sorted)))
         (flat (apply #'vector
                      (cl-loop for f in files
                               append (jj-diffedit-file-hunks f))))
         (instr-file (expand-file-name "JJ-INSTRUCTIONS" right))
         (instructions (when (file-readable-p instr-file)
                         (with-temp-buffer
                           (insert-file-contents instr-file)
                           (buffer-string)))))
    (jj-diffedit-session--make
     :left-dir left
     :right-dir right
     :files files
     :flat-hunks flat
     :current 0
     :instructions instructions)))

;;;; Display

(defun jj-diffedit--decision-label (hunk)
  "Return a propertized decision label for HUNK."
  (let* ((file (jj-diffedit-hunk-file hunk))
         (kind (jj-diffedit-file-kind file)))
    (cond
     ((eq kind 'ediff)
      (propertize "EDIFF" 'face 'jj-diffedit-ediff-face))
     ((eq (jj-diffedit-hunk-decision hunk) 'revert)
      (propertize "REVERT" 'face 'jj-diffedit-revert-face))
     (t (propertize "KEEP" 'face 'jj-diffedit-keep-face)))))

(defun jj-diffedit--progress-strip (session)
  "Return a string showing the per-hunk progress for SESSION."
  (let* ((flat (jj-diffedit-session-flat-hunks session))
         (current (jj-diffedit-session-current session)))
    (mapconcat
     (lambda (i)
       (let* ((h (aref flat i))
              (file (jj-diffedit-hunk-file h))
              (kind (jj-diffedit-file-kind file))
              (dec (jj-diffedit-hunk-decision h))
              (ch (cond ((eq kind 'ediff) "E")
                        ((eq dec 'revert) "R")
                        (t "K")))
              (face (cond ((= i current) 'highlight)
                          ((eq kind 'ediff) 'jj-diffedit-ediff-face)
                          ((eq dec 'revert) 'jj-diffedit-revert-face)
                          (t 'jj-diffedit-keep-face))))
         (propertize ch 'face face)))
     (number-sequence 0 (1- (length flat)))
     "")))

(defun jj-diffedit--render ()
  "Redraw the diffedit buffer from `jj-diffedit--session'."
  (let* ((session jj-diffedit--session)
         (flat (jj-diffedit-session-flat-hunks session))
         (n (length flat))
         (idx (jj-diffedit-session-current session)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ((zerop n)
        (insert (propertize "no changes between $left and $right\n"
                            'face 'jj-diffedit-header-face))
        (insert "\nC-c C-c to accept · C-c C-k to abort\n"))
       (t
        (let* ((hunk (aref flat idx))
               (file (jj-diffedit-hunk-file hunk))
               (path (jj-diffedit-file-path file))
               (kind (jj-diffedit-file-kind file))
               (sep (make-string 60 ?─)))
          (insert (propertize sep 'face 'jj-diffedit-header-face) "\n")
          (insert (propertize
                   (format "jj diffedit · hunk %d/%d · %s%s\n"
                           (1+ idx) n path
                           (pcase kind
                             ('added " [new]")
                             ('deleted " [deleted]")
                             ('binary " [binary]")
                             ('ediff " [ediff-resolved]")
                             (_ "")))
                   'face 'jj-diffedit-header-face))
          (insert "decision: "
                  (jj-diffedit--decision-label hunk)
                  " "
                  (propertize "u" 'face 'help-key-binding) "·revert "
                  (propertize "l" 'face 'help-key-binding) "·keep "
                  (propertize "e" 'face 'help-key-binding) "·ediff "
                  (propertize "n/p" 'face 'help-key-binding) "·nav "
                  (propertize "g" 'face 'help-key-binding) "·overview "
                  (propertize "?" 'face 'help-key-binding) "·help\n")
          (insert (propertize sep 'face 'jj-diffedit-header-face) "\n\n")
          (let ((body-start (point)))
            (insert (jj-diffedit-hunk-text hunk))
            (unless (eq (char-before) ?\n) (insert "\n"))
            (jj-diffedit--fontify-diff body-start (point)))
          (insert "\n")
          (insert (propertize sep 'face 'jj-diffedit-header-face) "\n")
          (insert "[" (jj-diffedit--progress-strip session) "]\n")
          (when (jj-diffedit-session-instructions session)
            (insert "\n"
                    (propertize "JJ-INSTRUCTIONS" 'face 'font-lock-comment-face)
                    "\n"
                    (propertize (jj-diffedit-session-instructions session)
                                'face 'font-lock-comment-face))))))
      (goto-char (point-min)))))

(defun jj-diffedit--fontify-diff (beg end)
  "Apply diff faces line-by-line to the region BEG..END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let* ((line-end (min (line-end-position) end))
             (face (cond
                    ((looking-at "^@@") 'diff-hunk-header)
                    ((looking-at "^\\(---\\|\\+\\+\\+\\) ") 'diff-header)
                    ((looking-at "^-") 'diff-removed)
                    ((looking-at "^\\+") 'diff-added)
                    ((looking-at "^ ") 'diff-context)
                    ((looking-at "^\\\\ ") 'diff-context))))
        (when face
          (put-text-property (point) line-end 'face face))
        (forward-line 1)))))

;;;; Navigation and decisions

(defun jj-diffedit--current-hunk ()
  (let* ((session jj-diffedit--session)
         (flat (jj-diffedit-session-flat-hunks session))
         (idx (jj-diffedit-session-current session)))
    (and (< idx (length flat))
         (aref flat idx))))

(defun jj-diffedit--goto (idx)
  (let* ((session jj-diffedit--session)
         (n (length (jj-diffedit-session-flat-hunks session))))
    (when (> n 0)
      (setf (jj-diffedit-session-current session)
            (mod idx n))
      (jj-diffedit--render))))

(defun jj-diffedit-next ()
  "Move to the next hunk."
  (interactive)
  (let ((idx (jj-diffedit-session-current jj-diffedit--session)))
    (jj-diffedit--goto (1+ idx))))

(defun jj-diffedit-previous ()
  "Move to the previous hunk."
  (interactive)
  (let ((idx (jj-diffedit-session-current jj-diffedit--session)))
    (jj-diffedit--goto (1- idx))))

(defun jj-diffedit-keep ()
  "Mark the current hunk as kept (included in the split commit) and advance."
  (interactive)
  (jj-diffedit--set-decision 'keep)
  (jj-diffedit-next))

(defun jj-diffedit-revert ()
  "Mark the current hunk as reverted (excluded from the split commit) and advance."
  (interactive)
  (jj-diffedit--set-decision 'revert)
  (jj-diffedit-next))

(defun jj-diffedit--set-decision (decision)
  (let ((hunk (jj-diffedit--current-hunk)))
    (when hunk
      (let ((file (jj-diffedit-hunk-file hunk)))
        (if (eq (jj-diffedit-file-kind file) 'ediff)
            (user-error "File %s is ediff-resolved; per-hunk decisions don't apply"
                        (jj-diffedit-file-path file))
          (setf (jj-diffedit-hunk-decision hunk) decision))))))

;;;; Ediff escape hatch

(defvar jj-diffedit--ediff-return nil
  "Internal: session to return to after ediff exits.")

(defun jj-diffedit-ediff ()
  "Resolve the current file via ediff.
Opens ediff between $left/path and $right/path. When ediff quits, the
right-side buffer is saved to $right/path and the file is marked
ediff-resolved (per-hunk decisions are ignored)."
  (interactive)
  (let* ((hunk (jj-diffedit--current-hunk))
         (file (and hunk (jj-diffedit-hunk-file hunk)))
         (kind (and file (jj-diffedit-file-kind file))))
    (unless file (user-error "No hunk at point"))
    (unless (memq kind '(modified binary))
      (user-error "Cannot ediff a %s file" kind))
    (let* ((session jj-diffedit--session)
           (lp (jj-diffedit-file-left-path file))
           (rp (jj-diffedit-file-right-path file))
           (parent-buf (current-buffer)))
      (setq jj-diffedit--ediff-return
            (list :session session :buffer parent-buf :file file))
      (ediff-files lp rp
                   (list (lambda ()
                           (add-hook 'ediff-quit-hook
                                     #'jj-diffedit--ediff-quit-hook
                                     nil t)))))))

(defun jj-diffedit--ediff-quit-hook ()
  "Save the right-side buffer and resume the diffedit session."
  (let* ((state jj-diffedit--ediff-return)
         (parent-buf (plist-get state :buffer))
         (file (plist-get state :file))
         (buf-b ediff-buffer-B))
    (when (and buf-b (buffer-live-p buf-b))
      (with-current-buffer buf-b
        (when (buffer-modified-p)
          (save-buffer))))
    (when file
      (setf (jj-diffedit-file-kind file) 'ediff))
    (setq jj-diffedit--ediff-return nil)
    (when (buffer-live-p parent-buf)
      (with-current-buffer parent-buf
        (jj-diffedit--render))
      (pop-to-buffer parent-buf))))

;;;; Overview buffer

(defvar jj-diffedit-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jj-diffedit-overview-goto)
    (define-key map (kbd "u") #'jj-diffedit-overview-revert)
    (define-key map (kbd "l") #'jj-diffedit-overview-keep)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'jj-diffedit-overview-refresh)
    map))

(define-derived-mode jj-diffedit-overview-mode tabulated-list-mode "JJ-Diffedit-Overview"
  "Tabular overview of all hunks in a jj-diffedit session."
  (setq tabulated-list-format
        [("Dec" 6 nil)
         ("Idx" 4 nil)
         ("File" 40 t)
         ("Hunk" 50 nil)])
  (tabulated-list-init-header))

(defvar-local jj-diffedit-overview--parent nil
  "Buffer of the diffedit session this overview belongs to.")

(defun jj-diffedit-overview ()
  "Open a tabular overview of all hunks in this session."
  (interactive)
  (let* ((session jj-diffedit--session)
         (parent (current-buffer))
         (buf (get-buffer-create
               (format "*jj-diffedit-overview: %s*"
                       (jj-diffedit-session-right-dir session)))))
    (with-current-buffer buf
      (jj-diffedit-overview-mode)
      (setq jj-diffedit-overview--parent parent)
      (jj-diffedit-overview--populate session)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun jj-diffedit-overview--populate (session)
  "Fill `tabulated-list-entries' from SESSION."
  (let* ((flat (jj-diffedit-session-flat-hunks session))
         entries)
    (dotimes (i (length flat))
      (let* ((h (aref flat i))
             (file (jj-diffedit-hunk-file h))
             (path (jj-diffedit-file-path file))
             (dec (jj-diffedit--decision-label h))
             (hunk-hdr (jj-diffedit-hunk-header h)))
        (push (list i (vector dec
                              (number-to-string (1+ i))
                              path
                              hunk-hdr))
              entries)))
    (setq tabulated-list-entries (nreverse entries))))

(defun jj-diffedit-overview-refresh ()
  "Refresh the overview from the parent session."
  (interactive)
  (when (buffer-live-p jj-diffedit-overview--parent)
    (jj-diffedit-overview--populate
     (buffer-local-value 'jj-diffedit--session jj-diffedit-overview--parent))
    (tabulated-list-print t)))

(defun jj-diffedit-overview-goto ()
  "Jump to the selected hunk in the parent buffer."
  (interactive)
  (let ((idx (tabulated-list-get-id)))
    (when (and idx (buffer-live-p jj-diffedit-overview--parent))
      (pop-to-buffer jj-diffedit-overview--parent)
      (jj-diffedit--goto idx))))

(defun jj-diffedit-overview-set (decision)
  (let ((idx (tabulated-list-get-id)))
    (when (and idx (buffer-live-p jj-diffedit-overview--parent))
      (let* ((session (buffer-local-value 'jj-diffedit--session
                                          jj-diffedit-overview--parent))
             (hunk (aref (jj-diffedit-session-flat-hunks session) idx))
             (file (jj-diffedit-hunk-file hunk)))
        (if (eq (jj-diffedit-file-kind file) 'ediff)
            (user-error "File %s is ediff-resolved" (jj-diffedit-file-path file))
          (setf (jj-diffedit-hunk-decision hunk) decision)
          (jj-diffedit-overview-refresh)
          (with-current-buffer jj-diffedit-overview--parent
            (jj-diffedit--render)))))))

(defun jj-diffedit-overview-keep ()
  "Mark hunk at point as kept."
  (interactive)
  (jj-diffedit-overview-set 'keep))

(defun jj-diffedit-overview-revert ()
  "Mark hunk at point as reverted."
  (interactive)
  (jj-diffedit-overview-set 'revert))

;;;; Save / abort

(defun jj-diffedit--apply-file (file)
  "Apply per-hunk decisions for FILE by mutating its right-side path."
  (let* ((kind (jj-diffedit-file-kind file))
         (lp (jj-diffedit-file-left-path file))
         (rp (jj-diffedit-file-right-path file))
         (hunks (jj-diffedit-file-hunks file)))
    (pcase kind
      ('ediff
       ;; The right-side buffer was already saved during the ediff session.
       nil)
      ('added
       ;; revert => the new file should not exist in the split commit
       (when (eq (jj-diffedit-hunk-decision (car hunks)) 'revert)
         (when (file-exists-p rp) (delete-file rp))))
      ('deleted
       ;; revert => the deletion should not happen in the split commit;
       ;; restore the file from $left into $right
       (when (eq (jj-diffedit-hunk-decision (car hunks)) 'revert)
         (make-directory (file-name-directory rp) t)
         (copy-file lp rp t)))
      ('binary
       (when (eq (jj-diffedit-hunk-decision (car hunks)) 'revert)
         (copy-file lp rp t)))
      ('modified
       (let ((reverts (cl-remove-if-not
                       (lambda (h) (eq (jj-diffedit-hunk-decision h) 'revert))
                       hunks)))
         (when reverts
           (jj-diffedit--patch-reverse rp reverts)))))))

(defun jj-diffedit--patch-reverse (target hunks)
  "Roll back HUNKS in TARGET file via `patch -R'."
  (let ((patch (make-temp-file "jj-diffedit-" nil ".patch"))
        (out (make-temp-file "jj-diffedit-out-")))
    (unwind-protect
        (progn
          (with-temp-file patch
            (insert "--- a\n+++ b\n")
            (dolist (h hunks)
              (insert (jj-diffedit-hunk-text h))
              (unless (eq (char-before) ?\n) (insert "\n"))))
          (with-temp-buffer
            (let ((exit (call-process jj-diffedit-patch-program nil t nil
                                      "-R" "--no-backup-if-mismatch"
                                      "--quiet"
                                      "-o" out
                                      "-i" patch
                                      target)))
              (unless (zerop exit)
                (error "patch -R failed (exit %s):\n%s"
                       exit (buffer-string)))))
          (copy-file out target t))
      (ignore-errors (delete-file patch))
      (ignore-errors (delete-file out)))))

(defun jj-diffedit-save ()
  "Apply all decisions to $right and exit."
  (interactive)
  (unless jj-diffedit--session
    (user-error "Not in a jj-diffedit session"))
  (let* ((session jj-diffedit--session)
         (right (jj-diffedit-session-right-dir session)))
    (dolist (file (jj-diffedit-session-files session))
      (condition-case err
          (jj-diffedit--apply-file file)
        (error
         (display-warning 'jj-diffedit
                          (format "applying %s failed: %s"
                                  (jj-diffedit-file-path file)
                                  (error-message-string err))
                          :error))))
    ;; jj will clean up its own JJ-INSTRUCTIONS, but if any earlier abort wrote
    ;; the sentinel, remove it.
    (let ((sentinel (expand-file-name jj-diffedit-abort-sentinel right)))
      (when (file-exists-p sentinel) (delete-file sentinel)))
    (jj-diffedit--exit 0)))

(defun jj-diffedit-abort ()
  "Abort the session, signalling jj to cancel."
  (interactive)
  (unless jj-diffedit--session
    (user-error "Not in a jj-diffedit session"))
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p "Abort this jj diffedit session? "))
    (let* ((session jj-diffedit--session)
           (right (jj-diffedit-session-right-dir session))
           (sentinel (expand-file-name jj-diffedit-abort-sentinel right)))
      (condition-case _ (with-temp-file sentinel (insert "1\n")) (error nil))
      (jj-diffedit--exit 1))))

(defun jj-diffedit--exit (code)
  "Release the editor with exit CODE.
In emacsclient mode this deletes the frame (releasing the client). In
standalone mode it kills emacs with CODE."
  (let* ((session jj-diffedit--session)
         (frame (and session (jj-diffedit-session-frame session)))
         (buffer (and session (jj-diffedit-session-buffer session))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)))
    (cond
     ((and frame (frame-live-p frame) (frame-parameter frame 'client))
      (delete-frame frame))
     ((daemonp)
      (when (and frame (frame-live-p frame)) (delete-frame frame))
      (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer)))
     (t (kill-emacs code)))))

;;;; Mode + entry point

(defvar jj-diffedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'jj-diffedit-next)
    (define-key map (kbd "p") #'jj-diffedit-previous)
    (define-key map (kbd "l") #'jj-diffedit-keep)
    (define-key map (kbd "u") #'jj-diffedit-revert)
    (define-key map (kbd "e") #'jj-diffedit-ediff)
    (define-key map (kbd "g") #'jj-diffedit-overview)
    (define-key map (kbd "C-c C-c") #'jj-diffedit-save)
    (define-key map (kbd "C-c C-k") #'jj-diffedit-abort)
    (define-key map (kbd "q") #'jj-diffedit-abort)
    (define-key map (kbd "?") #'describe-mode)
    map))

(define-derived-mode jj-diffedit-mode special-mode "JJ-Diffedit"
  "Major mode for interactively choosing hunks during `jj split -i'.
\\{jj-diffedit-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

;;;###autoload
(defun jj-diffedit (left right)
  "Interactively choose which hunks remain in $right (the split commit).
LEFT and RIGHT are the directories jj passes as `$left' and `$right'."
  (let* ((session (jj-diffedit--build-session left right))
         (frame (selected-frame))
         (buf (get-buffer-create
               (format "*jj-diffedit: %s*"
                       (file-name-nondirectory
                        (directory-file-name right))))))
    (setf (jj-diffedit-session-frame session) frame
          (jj-diffedit-session-buffer session) buf)
    (with-current-buffer buf
      (jj-diffedit-mode)
      (setq-local jj-diffedit--session session)
      (jj-diffedit--render))
    (switch-to-buffer buf)
    buf))

(provide 'jj-diffedit)
;;; jj-diffedit.el ends here
