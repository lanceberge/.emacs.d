---
name: package
description: Factor out emacs lisp to a custom package
---

# Package

Create or update a local Emacs Lisp package in `./lisp/<name>.el`.

Use the current task context first. If the user refers to "this", "that", "what you just wrote", or similar wording, infer the package from the work already in progress. If the user gives a package name or behavior directly, use that.

Keep the package small and focused:

- Put the package file at `lisp/<name>.el`.
- Prefer existing repo conventions over new structure.
- Name custom functions and variables with a leading `+`.
- All functions and variable names should be prefixed with `+<name>-`
- Private helper functions should be prefixed with `+<name>--`
- Add `;;;###autoload` immediately above custom `defun` declarations.
- Put interactive/core commands above helper functions they call.
- End the file with `(provide '<name>)`. The provide block should not include the "+" prefix
- Have a use package block somewhere in the editor subdirectory, typically editor.el but if there is another file where this functionality makes sense, then put it there, for example:
- If the package is requested to end in `-extras`, e.g. `+consult-extras`, then functions and variables should just be named `+consult-<function name>`. `-extras` should be omitted from the names

```
(use-package consult-extras
  :ensure (:type file :main "~/.emacs.d/lisp/consult-extras.el" :files "~/.emacs.d/lisp/consult-extras.el")
  :custom
  (consult-preview-excluded-buffers #'+consult-preview-tramp-excluded-p)
  :bind
  (:map +leader-map
    ("ab" . #'+consult-extras-example-function)))
```

- If there are hardcoded constants in the old code, create customizable variables with sensible defaults where applicable, but if there are already values configured, then have the use-package block set those values via :custom
- If there are already keybindings for the functions in this package, e.g. in another use-package block, then remove them from there and move them to the new use-package block
- If the functions got renamed from their previous values based on the `+<name>-` setup, rename all usages

## Finalizing changes

- Evaluate modified buffers
- If you subsequently run into issues, make new changes, evaluate those buffers, then run `emacsclient -e '(elpaca-rebuild "<new package name>") where the package name is from the provide call
