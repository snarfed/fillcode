#!/bin/bash
#
# Run the fillcode unit tests against different versions of GNU Emacs and XEmacs.
#
# This script assumes that it's running inside a directory hierarchy that
# looks like this:
#
# fillcode.el
# fillcode_unittest.el
# elunit/
#     |- elunit.el
#     |- runtests.sh

DIR=`dirname $0`/..
TESTED=0

cd $DIR

for prog in emacs21 emacs22 emacs23 xemacs21 emacs-24.4; do
    if which $prog >& /dev/null; then
        $prog -version 2> /dev/null | head -n 2
        # note princ instead of message below. we don't want message to
        # interpret any % char in the results as a string formatting
        # placeholder.
        exec $prog --batch -l "fillcode_unittest.el" \
            --eval "(progn (set-variable 'debug-on-error t)
                           (elunit-run (list \"fillcode_unittest.el\"))
                           (switch-to-buffer \"*Elunit Result*\")
                           (princ (buffer-string)))" \
            2>&1 | egrep -v 'Using the CPython shell|Mark set|^Loading'
        TESTED=1
    fi
done

if [ $TESTED == 0 ]; then
  echo "No emacsen found!"
fi
