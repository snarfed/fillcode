;; Fillcode
;; http://snarfed.org/space/fillcode
;; Copyright 2005 Ryan Barrett <fillcode@ryanb.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained at
;; http://www.gnu.org/licenses/gpl.html or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Unit tests for fillcode; run them with runtests.sh or M-x eval-buffer.
;; For more information about fillcode, see fillcode.el.
;; For more information about elunit, see http://lostway.org/~tko/elisp/elunit/


(if (not (member "." load-path))
    (setq load-path (cons "." load-path)))
(if (not (member "elunit" load-path))
    (setq load-path (cons "elunit" load-path)))

(require 'elunit)
(require 'fillcode)

(global-set-key [(control f10)]
  (lambda ()
    (interactive)
    (save-some-buffers)
    (elunit-run (list (buffer-name)))
    ))


; test harness. runs fillcode on the given input in a temp buffer (with the
; desired fill column, if provided), then compares the results to the expected
; output.
(defun fillcode-test (input expected &optional desired-fill-column)
  (with-temp-buffer
    (fillcode-mode)
    (insert-string input)
    (beginning-of-buffer)
    (if desired-fill-column
        (setq fill-column desired-fill-column))
    (fillcode)
    (assert-equal expected (buffer-string)))
  )

; error handling test harness. runs fillcode on the given input in a temp
; buffer, and succeeds only if fillcode raises the error no-function-to-fill.
(defun fillcode-test-error (input)
  (condition-case err
      (progn
        (fillcode-test input "")
        (fail "Expected error no-function-to-fill, but no error was raised"))

    (error (let ((msg (cadr err)))
             (if (not (equal msg "No function found to fill"))
                 (fail (concat "Unexpected error: " msg)))))
    ))


; test cases
(deftest no-function-to-fill
  (fillcode-test-error "")
  (fillcode-test-error ")")
  (fillcode-test-error "foo")
  (fillcode-test-error "foo)")
  )

(deftest no-args
  (fillcode-test "()" "()")
  (fillcode-test "foo()" "foo()")
  (fillcode-test "foo(\n)" "foo()")
  (fillcode-test "foo(\n\n)" "foo()")
  )

(deftest paren-whitespace
  (fillcode-test "foo( )" "foo()")
  (fillcode-test "foo(bar )" "foo(bar)")
  (fillcode-test "foo( bar)" "foo(bar)")
  (fillcode-test "foo( bar )" "foo(bar)")
  (fillcode-test "foo(bar, baz )" "foo(bar, baz)")
  (fillcode-test "foo( bar, baz)" "foo(bar, baz)")
  (fillcode-test "foo( bar, baz )" "foo(bar, baz)")
  )

(deftest comma-whitespace
  (fillcode-test "foo(bar,baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar,  baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar , baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar  ,  baz)" "foo(bar, baz)")
  )

(deftest paren-newlines
  (fillcode-test "foo(bar)" "foo(bar)")
  (fillcode-test "foo(bar\n)" "foo(bar)")
  (fillcode-test "foo(\nbar\n)" "foo(bar)")
  )

(deftest comma-newlines
  (fillcode-test "foo(bar,\nbaz)" "foo(bar, baz)")
  (fillcode-test "foo(bar\n,baz)" "foo(bar, baz)")
  (fillcode-test "foo(\nbar,baz\n)" "foo(bar, baz)")
  (fillcode-test "foo(\nbar\n,\nbaz\n)" "foo(bar, baz)")
  )

(deftest simple-fill
  (fillcode-test "foo(bar, baz)" "foo(bar,\n    baz)" 10)
  )
