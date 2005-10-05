#!/bin/sh

exec emacs --batch -q -l fillcode_unittest.el \
  --eval='(progn (elunit-run (list "fillcode_unittest.el"))
                 (switch-to-buffer "*Elunit Result*")
                 (message (buffer-string)))'
