## Rules
- After editing Emacs Lisp files, once you have finalized all changes, eval each modified buffer via `emacsclient`. Run this for each file you modified (substitute the absolute path):

```sh
emacsclient -e '(+agent-lisp-eval-buffer "/abs/path/to/file.el")'
```

- If you are editing a file in ./lisp, then once you are done with your changes, run `emacsclient -e '(elpaca-rebuild (quote <package name>))` where <package name> is the name of the package provided at the end
- Use jj for version control. Not git. Run `jj diff --git` after making changes to ensure the diff is as minimal as it needs to be
- All custom functions and variables, except eshell alias functions, should be named starting with `+`. Custom functions should have `;;;###autoload` on the line above the `defun` declaration.
- If you encounter changes you didn't make, assume the user made them and wants to keep them. Do not undo them
- Avoid advice wherever possible in favor of hooks/custom modes or commands
- Do not guess at the purpose/format of any functions or variables. Read docs and evaluate elisp to validate assumptions

## Look up documentation using `emacsclient`

```sh
# Function documentation
emacsclient -e '(substring-no-properties (documentation (symbol-function (quote example-symbol))))'

# Function source code
emacsclient -e '(+agent-lisp-function-source "example-symbol")'

# Variable value
emacsclient -e '(symbol-value (quote example-variable))'

# Variable documentation
emacsclient -e '(substring-no-properties (documentation-property (quote example-variable) (quote variable-documentation)))'

# View matching functions and variables
emacsclient -e '(+agent-lisp-apropos "pattern")'
```

## View Package source code

View the source for packages in ./elpaca/sources

## Style

Put core functions e.g. commands above helper functions that they call, never below
