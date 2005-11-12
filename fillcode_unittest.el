;; fillcode_unittest.el - Unit tests for the fillcode minor mode
;;
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


; test harness. runs fillcode on the given input in a temp buffer in java-mode
; - with the desired fill column, if provided - then compares the results to
; the expected output.
;
; if the first character of the expected output is a newline, it's removed.
;
; returns the value returned from fillcode.
(defun fillcode-test (input expected &optional desired-fill-column)
  (let ((expected-trimmed
         (if (eq ?\n (string-to-char expected))
             (substring expected 1)
           expected)))
    (with-temp-buffer
      (fundamental-mode)
      (if (auto-fill-mode nil)  ; turn off auto-fill-mode so that the input
          (auto-fill-mode nil)) ; string isn't automatically filled
      (insert-string input)
      (java-mode-clean)       ; so that we know how to indent
      (beginning-of-buffer)
      (if desired-fill-column
          (setq fill-column desired-fill-column))
      (let ((ret (fillcode-fill-paragraph nil)))
        (assert-equal expected-trimmed (buffer-string))
        ret))
  ))

; set up java-mode appropriate for testing fillcode - plain vanilla (no
; hooks), no tabs, basic-offset 2.
(defun java-mode-clean ()
  (setq c-basic-offset 2
        indent-tabs-mode nil)
  (let ((java-mode-hook nil))
    (java-mode)))

; failure test harness. runs fillcode on the given input in a temp buffer, and
; succeeds only if fillcode returns nil (ie it didn't fill).
(defun fillcode-test-not-filled (input)
  (if (fillcode-test input input)
      (fail "Expected nil, but returned non-nil")))


; test cases
(deftest no-function-to-fill
  (fillcode-test-not-filled "")
  (fillcode-test-not-filled ")")
  (fillcode-test-not-filled "foo")
  (fillcode-test-not-filled "foo)")
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
  (fillcode-test "foo(bar
)
baz" "foo(bar)
baz")
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

(deftest blank-lines
  (fillcode-test "foo(\n\nbar, baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar\n\n,baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar,\n\nbaz)" "foo(bar, baz)")
  (fillcode-test "foo(\nbar, baz\n\n)" "foo(bar, baz)")

  (fillcode-test "foo(\n\nbar, baz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(bar\n\n,baz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(bar,\n\nbaz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(\nbar, baz\n\n)" "foo(bar,\n    baz)" 9)
  )

(deftest simple-fill
  (fillcode-test "foo(bar, baz)" "
foo(bar,
    baz)" 10)

  ; a
  (fillcode-test "foo(bar, baz, baj)" "
foo(bar,
    baz,
    baj)" 10)

  ; z
  (fillcode-test "foo(bar, baz, baj)" "
foo(bar,
    baz,
    baj)" 11)

  ; ,
  (fillcode-test "foo(bar, baz, baj)" "
foo(bar,
    baz,
    baj)" 12)

  ; [space]
  (fillcode-test "foo(bar, baz, baj)" "
foo(bar, baz,
    baj)" 13)

  ; b
  (fillcode-test "foo(bar, baz, baj)" "
foo(bar, baz,
    baj)" 14)

  ; if no fill point before fill-column, don't try to fill. wait until the
  ; next fill point.
  (fillcode-test "foo(bar, bazbaz, baj)" "
foo(bar,
    bazbaz,
    baj)" 10)
  )

(deftest multiple-identifiers-between-commas
  (fillcode-test "foo(bar baz, baj baf)" "
foo(bar baz,
    baj baf)" 18)

  (fillcode-test "foo(bar baz baj, baf bat bap)" "
foo(bar baz baj,
    baf bat bap)" 22)
  )

(deftest nested-non-sticky
  (setq fillcode-nested-calls-are-sticky nil)

  (fillcode-test "foo(x(y, z))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y ,z ))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y,z ) ,a( b ,c ))" "foo(x(y, z), a(b, c))")

  (fillcode-test "foo(bar, baz)" "
foo(
    bar,
    baz)" 6)

  (fillcode-test "foo(barbar, baz(baj))" "
foo(barbar,
    baz(baj))" 13)

  (fillcode-test "foo(barbar(baz))" "foo(barbar(
        baz))" 12)

  ; try with the fill column on different parts of the nested function call.
  ; the full text is:  foo(barbarbar, baz(x), baf)
  ;
  ; z
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 17)

  ; (
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 18)

  ; x
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(
        x), baf)" 19)

  ; )
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(
        x), baf)" 20)

  ; ,
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(
        x), baf)" 21)

  ; [space]
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(x),
    baf)" 22)

  ; b
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(x),
    baf)" 23)
  )


(deftest nested-sticky
  (set-variable 'fillcode-nested-calls-are-sticky t)

  (fillcode-test "foo(x(y, z))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y ,z ))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y,z ) ,a( b ,c ))" "foo(x(y, z), a(b, c))")

  (fillcode-test "foo(bar, baz)" "
foo(bar,
    baz)" 6)

  (fillcode-test "foo(barbar, baz(baj))" "
foo(barbar,
    baz(baj))" 13)

  ; sticky. shouldn't fill even though it extends beyond fill-column.
  (fillcode-test "foo(barbar(baz))" "foo(barbar(baz))" 12)

  ; (
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 18)

  ; x
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 19)

  ; )
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 20)

  ; ,
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar,
    baz(x), baf)" 21)

  ; [space]
  (fillcode-test "foo(barbarbar, baz(x), baf)" "
foo(barbarbar, baz(x),
    baf)" 22)
  )
