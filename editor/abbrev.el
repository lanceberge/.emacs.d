;;; -*- lexical-binding: t -*-
(use-package abbrev
  :ensure nil
  :hook
  ((elixir-mode elixir-ts-mode java-mode java-ts-mode) . abbrev-mode)
  :custom
  (save-abbrevs nil)
  :bind
  (:map +insert-mode-map
        ("M-/" . #'+expand-abbrev-or-dabbrev))
  :config
  (define-abbrev-table 'elixir-mode-abbrev-table
    '(
      ("defp" "defprotocol")
      ("defi" "defimpl")

      ("ioi" "IO.inspect")
      ("iop" "IO.puts")
      ("dbg" "dbg()")

      ("ecto" "use Ecto.Schema")
      ("hasm" "has_many")
      ("haso" "has_one")
      ("belto" "belongs_to")
      ("valr" "validate_required")
      ("valu" "validate_unique")
      ("assoc" "cast_assoc")

      ("lv" "use Phoenix.LiveView")
      ("heex" "~H\"\"\"")

      (";ok" "{:ok,")
      (";err" "{:error,")
      (";norep" "{:noreply, socket}")
      (";reply" "{:reply,")
      (";cont" "{:cont,")))

  (defvar elixir-ts-mode-abbrev-table)
  (setq elixir-ts-mode-abbrev-table elixir-mode-abbrev-table)

  (define-abbrev-table 'java-mode-abbrev-table
    '(
      ("sout" "System.out.println")
      ("serr" "System.err.println")
      ("souf" "System.out.printf")

      ("pub" "public")
      ("priv" "private")
      ("prot" "protected")
      ("stat" "static")
      ("fin" "final")
      ("abst" "abstract")
      ("syn" "synchronized")

      ("str" "String")
      ("bool" "boolean")
      ("arrl" "ArrayList")
      ("ll" "LinkedList")
      ("hm" "HashMap")
      ("hs" "HashSet")

      ("impl" "implements")
      ("ext" "extends")
      ("ret" "return")
      ("thr" "throws")

      ("ov" "@Override")
      ("test" "@Test")))

  (defvar java-ts-mode-abbrev-table)
  (setq java-ts-mode-abbrev-table java-mode-abbrev-table)

  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(
      ("ci" "call-interactively #'"))))

;;;###autoload
(defun +expand-abbrev-or-dabbrev ()
  (interactive)
  (unless (expand-abbrev)
    (dabbrev-expand nil)))
