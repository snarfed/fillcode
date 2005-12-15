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

;; Unit tests for fillcode; run them with runtests.sh or M-x eval-buffer C-F10.
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
    (elunit-run '("fillcode_unittest.el"))
    ))


; test harness. runs fillcode on the given input in a temp buffer, once in
; python mode and once in java mode. before running it in java mode, it
; appends ; to the input string. the fill column is set to desired-fill-columnm
; if provided.
;
; then, asserts that the results equal the expected output. (if the first
; character of the expected output is a newline, it's removed.)
;
; returns the value returned from fillcode.
(defun fillcode-test (input expected &optional desired-fill-column)
  (fillcode-test-in-mode input expected
                         'python-mode desired-fill-column)
  (fillcode-test-in-mode (concat input ";") (concat expected ";")
                         'java-mode desired-fill-column)
  )

;; actually set up the buffer, run fillcode, and check the output
(defun fillcode-test-in-mode (input expected mode desired-fill-column)
  (let ((expected-trimmed
         (if (eq ?\n (string-to-char expected))
             (substring expected 1)
           expected)))
    (with-temp-buffer
      (toggle-mode-clean 'fundamental-mode)
      (insert-string input)
      (toggle-mode-clean mode)
      (beginning-of-buffer)
      (if desired-fill-column
          (setq fill-column desired-fill-column))
      (let ((ret (fillcode-fill-paragraph nil)))
        (assert-equal expected-trimmed (buffer-string))
        ret))
    ))

; turn on the given major mode, set up so it's appropriate for testing
; fillcode: plain vanilla (no hooks), no tabs, basic-offset 2.
(defun toggle-mode-clean (mode)
  (let ((python-mode-hook nil)
        (c-mode-common-hook nil)
        (perl-mode-hook nil)
        (shell-mode-hook nil)
        (sql-mode-hook nil))
    (funcall mode))
  (setq indent-tabs-mode nil
;;         c-basic-offset 2
        )
  (fillcode-mode))

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

  ;; should know when to stop even if parenthetical expression is blank
  (fillcode-test "foo() bar( )" "foo() bar()")
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

(deftest arithmetic-whitespace
  (fillcode-test "foo(bar+baz)" "foo(bar + baz)")
  (fillcode-test "foo(bar-  baz)" "foo(bar - baz)")
  (fillcode-test "foo(bar /baz)" "foo(bar / baz)")
  (fillcode-test "foo(bar  *  baz)" "foo(bar * baz)")
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
  (setq fillcode-open-paren-sticky nil)

  (fillcode-test "foo(x(y, z))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y ,z ))" "foo(x(y, z))")
  (fillcode-test "foo( x ( y,z ) ,a( b ,c ))" "foo(x(y, z), a(b, c))")

  (fillcode-test "foo(bar,baz)" "
foo(
    bar,
    baz)" 6)

  (fillcode-test "foo(barbar, baz(baj))" "
foo(barbar,
    baz(baj))" 13)

  (fillcode-test "foo(barbar(baz))" "
foo(barbar(
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
  (set-variable 'fillcode-open-paren-sticky t)

  (fillcode-test "foo(bar,baz)" "
foo(bar,
    baz)" 6)
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

(deftest arithmetic
  (fillcode-test "foo(bar + baz)" "foo(bar + baz)" 16)
  (fillcode-test "foo(bar - baz)" "foo(bar - baz)" 16)
  (fillcode-test "foo(bar / baz)" "foo(bar / baz)" 16)
  (fillcode-test "foo(bar * baz)" "foo(bar * baz)" 16)
  (fillcode-test "foo(bar == baz)" "foo(bar == baz)" 16)
  (fillcode-test "foo(bar != baz)" "foo(bar != baz)" 16)
  (fillcode-test "foo(bar >= baz)" "foo(bar >= baz)" 16)
  (fillcode-test "foo(bar <= baz)" "foo(bar <= baz)" 16)
  (fillcode-test "foo(bar + baz)" "
foo(bar +
    baz)" 11)
  (fillcode-test "foo(bar - baz)" "
foo(bar -
    baz)" 11)
  (fillcode-test "foo(bar / baz)" "
foo(bar /
    baz)" 11)
  (fillcode-test "foo(bar * baz)" "
foo(bar *
    baz)" 11)
  (fillcode-test "foo(bar == baz)" "
foo(bar ==
    baz)" 11)
  (fillcode-test "foo(bar != baz)" "
foo(bar !=
    baz)" 11)
  (fillcode-test "foo(bar >= baz)" "
foo(bar >=
    baz)" 11)
  (fillcode-test "foo(bar <= baz)" "
foo(bar <=
    baz)" 11)
  (fillcode-test "foo(bar + baz - baf / baj * bap)" "
foo(bar +
    baz -
    baf /
    baj *
    bap)" 11)
  (fillcode-test "foo(bar+baz)" "
foo(bar +
    baz)" 6)
  )


(deftest multiple-parenthesized-expressions
  ;; if there are multiple top-level parenthetic expressions, we should fill
  ;; all of them, not just the first
  (fillcode-test "foo(bar) foo(baz,baj)" "foo(bar) foo(baz, baj)")
  (fillcode-test "foo(bar) foo(baz,baj)" "
foo(bar) foo(baz,
             baj)" 18)

  ;; ...even if they span multiple lines. (or not. TODO for later maybe.)
;;   (fillcode-test "if (bar) \\\n  foo(baz,baj)" "
;; if (bar) \\
;;   foo(baz,
;;       baj)" 12)
  )


(deftest non-fill-points
  ;; make sure that tokens aren't normalized or filled at other special tokens
  (fillcode-test "foo(bar.baz)" "foo(bar.baz)" 6)
  (fillcode-test "foo(bar_baz)" "foo(bar_baz)" 6)
  (fillcode-test "foo(bar%baz)" "foo(bar%baz)" 6)
  (fillcode-test "foo(bar$baz)" "foo(bar$baz)" 6)
  (fillcode-test "foo(bar~baz)" "foo(bar~baz)" 6)
  (fillcode-test "foo(bar`baz)" "foo(bar`baz)" 6)
  (fillcode-test "foo(bar@baz)" "foo(bar@baz)" 6)
  (fillcode-test "foo(bar!baz)" "foo(bar!baz)" 6)
  (fillcode-test "foo(bar:baz)" "foo(bar:baz)" 6)
  (fillcode-test "foo(bar?baz)" "foo(bar?baz)" 6)
  (fillcode-test-in-mode "foo(bar#baz);" "foo(bar#baz);" 'java-mode 6)
  (fillcode-test "foo(bar->baz)" "foo(bar->baz)" 6)
  (fillcode-test "foo(bar *baz)" "foo(bar *baz)" 6)  ;; pointers in c and c++
  (fillcode-test "foo(bar* baz)" "foo(bar* baz)" 6)
  (fillcode-test "foo(bar*baz)" "foo(bar*baz)" 6)
  )

(deftest literals
  ;; string literals and comments should be kept intact and treated as single,
  ;; unbreakable tokens, not normalized or filled inside
  (fillcode-test "foo(\"bar,baz\")" "foo(\"bar,baz\")")
  (fillcode-test-in-mode "foo(\"bar,baz\")" "foo(\"bar,baz\")" 'java-mode 20)

  (fillcode-test "foo(\"bar,baz\")" "foo(\"bar,baz\")" 6)
  (fillcode-test-in-mode "foo('bar,baz')" "foo('bar,baz')" 'java-mode 6)

  (fillcode-test "foo(\"bar\" + baz + \"baj\")" "
foo(\"bar\" +
    baz +
    \"baj\")" 12)

  (fillcode-test "foo(\"bar + bar\" + baz + \"baj + baj\")" "
foo(\"bar + bar\" +
    baz +
    \"baj + baj\")" 12)

  ; don't fill whole-line comments (# and //)
  (fillcode-test-in-mode "foo(bar) // baz, baj" "foo(bar) // baz, baj"
                         'java-mode 16)
  ; emacs 21's python.el doesn't set `fill-paragraph-function', so it doesn't
  ; fill this line...but emacs 22's python.el does. i haven't yet figured out
  ; how to make this test portable. :/
;;   (fillcode-test "foo(bar) # baz, baj" "foo(bar) # baz, baj" 16)

   (fillcode-test-in-mode "foo(bar, /*baz ,baj*/, bax)" "
foo(bar,
    /*baz ,baj*/,
    bax)" 'java-mode 6)

  ;; TODO: get c++ comments working
;;   (fillcode-test-in-mode "
;; foo(// bar, baz
;;    bajbaj, bax)" "
;; foo(// bar, baz
;;     bajbaj,
;;     bax)" 'java-mode 12)

  ;; literals should still be normalized *around*, though
  (fillcode-test "foo(\"bar\",\"baz\")" "foo(\"bar\", \"baz\")")
  )

(deftest fillcode-end-of-statement
  ; point is at the beginning of the buffer, so *only* the first statement
  ; should be filled
  (fillcode-test "foo(y)\nbar( x)" "foo(y)\nbar( x)")

  ; open parens after fill points shouldn't trip us up
  (fillcode-test "foo(x, (y))\nbar( x)" "foo(x, (y))\nbar( x)")
  )



(defun inside-test (contents string point inside)
  (with-temp-buffer
    (insert-string contents)
    (goto-char point)
    (assert-equal inside (fillcode-inside string)
                  (concat "buffer: " contents ", string: " string))
    ))

(deftest inside
  ; base cases
  (inside-test "" "" 1 nil)
  (inside-test "a" "" 1 nil)
  (inside-test "ab" "" 1 nil)
  (inside-test "" "a" 1 nil)
  (inside-test "" "ab" 1 nil)

  ; one-char tests
  (inside-test "b" "a" 1 nil)
  (inside-test "a" "a" 1 t)
  (inside-test "ab" "a" 1 t)
  (inside-test "ab" "a" 2 nil)
  (inside-test "ba" "a" 1 nil)
  (inside-test "ba" "a" 2 t)
  (inside-test "aba" "a" 1 t)
  (inside-test "aba" "a" 2 nil)
  (inside-test "aba" "a" 3 t)
  (inside-test "aba" "b" 1 nil)
  (inside-test "aba" "b" 2 t)
  (inside-test "aba" "b" 3 nil)

  ; multiple-char tests
  (inside-test "ab" "ab" 1 t)
  (inside-test "ab" "ab" 2 t)
  (inside-test "ac" "ab" 1 nil)
  (inside-test "ac" "ab" 2 nil)
  (inside-test "cb" "ab" 1 nil)
  (inside-test "cb" "ab" 2 nil)

  (inside-test "abc" "ab" 1 t)
  (inside-test "abc" "ab" 2 t)
  (inside-test "abc" "ab" 3 nil)
  (inside-test "abc" "bc" 1 nil)
  (inside-test "abc" "bc" 2 t)
  (inside-test "abc" "bc" 3 t)

  (inside-test "abc" "abc" 1 t)
  (inside-test "abc" "abc" 2 t)
  (inside-test "abc" "abc" 3 t)
  )
