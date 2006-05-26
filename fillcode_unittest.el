;; fillcode_unittest.el - Unit tests for the fillcode minor mode
;;
;; Fillcode
;; http://snarfed.org/space/fillcode
;; Copyright 2005-2006 Ryan Barrett <fillcode@ryanb.org>
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

;; Unit tests for fillcode; run them with M-x eval-buffer C-F10 or
;; ./elunit/runtests.sh.
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
    (elunit-run '("fillcode_unittest.el"))))


; test harness. runs fillcode on the given input in a temp buffer in python,
; java, and c++ major modes. appends semicolons in java-mode and open curly
; braces in c++-mode.
;
; then, asserts that the results equal the expected output. (if the first
; character of the expected output is a newline, it's removed.)
;
; if `desired-fill-column' is provided, `fill-column' is set to it.
;
; the expected output is normalized unless `dont-normalize' is non-nil.
;
; if `prefix-arg' is provided, it is passed as the first argument to
; `fill-paragraph-function'.
;
; returns the value returned from fillcode.
(defun fillcode-test (input expected &optional desired-fill-column
                            dont-normalize prefix-arg)
  (let ((expected (trim-leading-newlines expected)))
    (fillcode-test-in-mode input
                           (if dont-normalize expected
                             (normalize-python-indentation expected))
                           'python-mode desired-fill-column)
    (fillcode-test-in-mode (concat input ";") (concat expected ";")
                           'java-mode desired-fill-column)
    (fillcode-test-in-mode (concat input " {") (concat expected " {")
                           'c++-mode desired-fill-column)))


;; set up the buffer and mode, then run and check fillcode with point at the
;; beginning of the buffer, at the end, and in the middle
(defun fillcode-test-in-mode (input expected mode desired-fill-column)
  (dolist (point-fn (list
    'beginning-of-buffer
    'end-of-line-with-first-close-paren
    (lambda () (end-of-line-with-first-close-paren) (forward-line -1))))

    (with-temp-buffer
      (toggle-mode-clean 'fundamental-mode)
      (toggle-mode-clean mode)
      (if desired-fill-column
          (setq fill-column desired-fill-column))
      (insert-string input)
      (funcall point-fn)
      (fillcode-fill-paragraph prefix-arg)
      (assert-equal expected (buffer-string)))))


; these are self-explanatory
(defun end-of-line-with-first-close-paren ()
  (beginning-of-buffer)
  (search-forward ")" nil t)
  (end-of-line))

(defun trim-leading-newlines (string)
  (if (eq 0 (string-match "\n+" string))
      (replace-match "" t t string)
    string))


; the python-mode.el python mode (maintained at python.org and included with
; xemacs and emacs 21) is dumb about indentation. it doesn't recognize nested
; parenthetical expressions.
;
; the python.el python mode (included with emacs 22) is marginally smart. it's
; also unlike java-mode, though; it only indents four more spaces per nesting
; level, *not* to the open paren column, like it kinda should.
;
; so, if we're using python-mode and a line is indented more than four columns,
; normalize it to four columns if using python-mode.el, eight columns if using
; python.el. (this depends on the fact that the function call on the first line
; is always "foo(" and is not indented.)
(defun normalize-python-indentation (string)
  (if (and ;(eq major-mode 'python-mode)
       (string-match "\n[ ]\\{5,\\}" string))
      (replace-match
       (if (functionp 'py-version) "\n    " "\n        ")
       t t string)
    string))


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
        py-indent-offset 4
        c-basic-offset 4)
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
  )

(deftest fill-first-line-but-no-more
  ;; should stop filling at the end of a line, even if a parenthetical
  ;; expression is blank or has trailing spaces. (those were bugs. :P)
  (fillcode-test "foo() bar( )" "foo() bar()")
  (fillcode-test "foo()\nbar( x )" "foo()\nbar( x )")
  (fillcode-test "foo(x )\nbar(y)" "foo(x)\nbar(y)")
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
  (fillcode-test "foo(bar\n  )" "foo(bar)")
  (fillcode-test "foo(\n  bar)" "foo(bar)")
  (fillcode-test "foo(\n  bar\n  )" "foo(bar)")
  )

(deftest comma-newlines
  (fillcode-test "foo(bar,\n  baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar\n  ,baz)" "foo(bar, baz)")
  (fillcode-test "foo(\n  bar,baz\n  )" "foo(bar, baz)")
  (fillcode-test "foo(\n  bar\n  ,\n  baz\n  )" "foo(bar, baz)")
  )

(deftest arithmetic-whitespace
  (fillcode-test "foo(bar+baz)" "foo(bar + baz)")
  (fillcode-test "foo(bar-  baz)" "foo(bar - baz)")
  (fillcode-test "foo(bar /baz)" "foo(bar / baz)")
  (fillcode-test "foo(bar  *  baz)" "foo(bar * baz)")
  )

(deftest blank-lines
  (fillcode-test "foo(\n\n  bar, baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar\n\n  ,baz)" "foo(bar, baz)")
  (fillcode-test "foo(bar,\n\n  baz)" "foo(bar, baz)")
  (fillcode-test "foo(\n  bar, baz\n\n  )" "foo(bar, baz)")

  (fillcode-test "foo(\n\n  bar, baz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(bar\n\n  ,baz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(bar,\n\n  baz)" "foo(bar,\n    baz)" 9)
  (fillcode-test "foo(\n  bar, baz\n\n  )" "foo(bar,\n    baz)" 9)
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

  ; the minus sign is tricky. when it's used to indicate a negative scalar, it
  ; *shouldn't* be normalized.
  (fillcode-test "foo(-bar)" "foo(-bar)")
  (fillcode-test "foo(-3)" "foo(-3)")
  (fillcode-test "foo(bar,-baz)" "foo(bar, -baz)")
  (fillcode-test "foo(bar,-3)" "foo(bar, -3)")
  )


(deftest multiple-parenthesized-expressions
  ;; if there are multiple top-level parenthetic expressions, we should fill
  ;; all of them, not just the first
  (fillcode-test "foo(bar) foo(baz,baj)" "foo(bar) foo(baz, baj)")
  (fillcode-test "foo(bar) foo(baz,baj)" "
foo(bar) foo(baz,
             baj)" 18 t)

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

   (fillcode-test-in-mode "foo(bar, /*baz ,baj*/, bax)" "foo(bar,
    /*baz ,baj*/,
    bax)" 'java-mode 6)

   (fillcode-test-in-mode "foo(bar, #baz ,baj,\nbax)" "foo(bar,
    #baz ,baj,
    bax)" 'python-mode 6)

   (fillcode-test-in-mode "foo(bar, //baz ,baj,\nbax);" "foo(bar,
    //baz ,baj,
    bax);" 'c++-mode 6)


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

; if there's a prefix argument, fill at all top-level fill points. fill at
; other fill points only as needed.
(deftest prefix-argument
  (test-prefix-argument t)
  (test-prefix-argument nil))

(defun test-prefix-argument (sticky)
  (set-variable 'fillcode-open-paren-sticky sticky)

  (fillcode-test "foo(bar)" "foo(bar)" 80 nil t)

  (fillcode-test "foo(bar,baz)" "
foo(bar,
    baz)" 80 nil t)

  (fillcode-test "foo(bar,baz)" "
foo(bar,
    baz)" 12 nil t)

  (fillcode-test "foo(bar,baz(baj))" "
foo(bar,
    baz(baj))" 80 nil t)

  (fillcode-test "foo(bar,baz(baj, bak))" "
foo(bar,
    baz(baj, bak))" 80 nil t)

  (fillcode-test "foo(baz(baj, bak), bar)" "
foo(baz(baj, bak),
    bar)" 80 nil t))


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
