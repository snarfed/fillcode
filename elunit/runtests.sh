#!/bin/bash
#
# Run the fillcode unit tests against both gnu emacs and xemacs.
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

for prog in emacs xemacs  ~/emacs/src/emacs ~/xemacs/src/xemacs; do
    if which $prog >& /dev/null; then
        $prog -version 2> /dev/null | head -n 2
        exec $prog --batch -l "fillcode_unittest.el" \
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
