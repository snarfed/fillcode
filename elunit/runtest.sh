#!/bin/bash

TESTED=0

for prog in emacs xemacs; do
    if which $prog >& /dev/null; then
        $prog -version 2> /dev/null | head -n 2
        exec $prog --batch -q -l fillcode_unittest.el \
            --eval '(progn (elunit-run (list "fillcode_unittest.el"))
                           (switch-to-buffer "*Elunit Result*")
                           (message (buffer-string)))' \
            2>&1 | egrep -v 'Using the CPython shell|Mark set|^Loading'
        TESTED=1
    fi
done

if [ $TESTED == 0 ]; then
  echo "No emacsen found!"
fi
