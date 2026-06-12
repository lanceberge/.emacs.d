After editing files, once you have finalized all changes, eval each modified buffer via `emacsclient`.

Run this for each file you modified (substitute the absolute path):

```sh
emacsclient -e '(with-current-buffer (or (get-file-buffer "/abs/path/to/file.el") (find-file-noselect "/abs/path/to/file.el")) (revert-buffer t t t) (eval-buffer) "ok")'
```

All custom functions and variables should be named starting with `+` and should have `;;;###autoload` on the line above the `defun` declaration

## Look up documentation using `emacsclient`

```sh
# Function -- shows source code and docstring
emacsclient -e '(helpful-function #'example-symbol)

# Variable -- shows what it's set to and the docstring
emacsclient -e '(helpful-variable #'example-variable)

# View all functions and variables beginning with pattern
emacsclient -e '(apropos "pattern")
```

## View Package source code

View the source for packages in ./elpaca/sources
