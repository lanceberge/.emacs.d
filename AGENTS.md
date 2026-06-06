After editing any files, eval each modified buffer via `emacsclient`. Do this for every edit.

Run this for each file you modified (substitute the absolute path):

```sh
emacsclient -e '(with-current-buffer (or (get-file-buffer "/abs/path/to/file.el") (find-file-noselect "/abs/path/to/file.el")) (revert-buffer t t t) (eval-buffer) "ok")'
```
