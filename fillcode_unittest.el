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
; if `expected' is not provided, `input' is not expected to change.
;
; if `fill-col' is provided, `fill-column' is set to it.
;
; if `prefix-arg' is provided, it is passed as the first argument to
; `fill-paragraph-function'.
;
; returns the value returned from fillcode.
(defun fillcode-test (input &optional expected fill-col prefix-arg)
  (dolist (mode '(java-mode c++-mode python-mode))
    (fillcode-test-in-mode input expected mode fill-col prefix-arg)))


;; set up the buffer and mode, then run and check fillcode with point at the
;; beginning, end, and middle of the first statement
(defun fillcode-test-in-mode (input expected mode
                              &optional fill-col prefix-arg)
  ; add a statement *after* the current one so the mode's beginning- and
  ; end-of statement functions work
  (let* ((input (concat input "\nbar;"))
         (expected (if (not expected) input
                     ; trim leading newlines
                     (concat (string-replace expected "\\`\n+" "") "\nbar;"))))
    (dolist (point-fn (list
                       'beginning-of-buffer
                       'first-semicolon-or-open-brace
                       (lambda () (first-semicolon-or-open-brace)
                         (goto-char (max (point-min) (- (point) 4))))))
      (with-temp-buffer
        (toggle-mode-clean 'fundamental-mode)
        (toggle-mode-clean mode)
        (insert-string input) ; *after* setting mode
        (funcall point-fn)    ; *after* inserting input :P
        ; remove semicolons and braces for python. (have to do it here
        ; because point-fn depends on the semicolons.)
        (if (eq mode 'python-mode)
            (progn
              (buffer-replace ";\\|{\\|}" "")
              (setq expected (normalize-python-indentation
                              (string-replace expected ";\\|{\\|}" "")))))
        (setq fill-column (if fill-col fill-col 80))
        (fillcode-fill-paragraph prefix-arg)
        (assert-equal expected (buffer-string))))))

; replace all occurrences of regexp in string. returns the result string.
(defun string-replace (string regexp replacement)
  (set-match-data nil)
  (while (string-match regexp string)
    (setq string (replace-match replacement t nil string)))
  string)

; same as string-replace, but in the current buffer. (i originally used
; perform-replace, but its argument list differs between emacs versions!)
(defun buffer-replace (regexp replacement)
  (save-excursion
    (let ((new (string-replace (buffer-string) regexp replacement)))
      (erase-buffer)
      (insert new))))

; move point to the first semicolon or open brace in the buffer
(defun first-semicolon-or-open-brace ()
  (beginning-of-buffer)
  (re-search-forward ";\\|{" nil t)
  (end-of-line))


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
  string
;;   (string-replace string
;;                   "\n[ ]\\{5,\\}"
;;                   (if (functionp 'py-version) "\n    " "\n        "))
  )


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
;;   (fillcode-test "")
  (fillcode-test ";")
  (fillcode-test ");")
  (fillcode-test "foo;")
  (fillcode-test "foo);"))

(deftest no-args
  (fillcode-test "();")
  (fillcode-test "foo();")
  (fillcode-test "foo(\n);" "foo();"))

(deftest paren-whitespace
  (fillcode-test "foo( );" "foo();")
  (fillcode-test "foo(bar );" "foo(bar);")
  (fillcode-test "foo( bar);" "foo(bar);")
  (fillcode-test "foo( bar );" "foo(bar);")
  (fillcode-test "foo(bar, baz );" "foo(bar, baz);")
  (fillcode-test "foo( bar, baz);" "foo(bar, baz);")
  (fillcode-test "foo( bar, baz );" "foo(bar, baz);")
  (fillcode-test "foo(bar\n);" "foo(bar);"))

(deftest comma-whitespace
  (fillcode-test "foo(bar,baz);" "foo(bar, baz);")
  (fillcode-test "foo(bar,  baz);" "foo(bar, baz);")
  (fillcode-test "foo(bar , baz);" "foo(bar, baz);")
  (fillcode-test "foo(bar  ,  baz);" "foo(bar, baz);"))

(deftest paren-newlines
  (fillcode-test "foo(bar);" "foo(bar);")
  (fillcode-test "foo(bar\n  );" "foo(bar);")
  (fillcode-test "foo(\n  bar);" "foo(bar);")
  (fillcode-test "foo(\n  bar\n  );" "foo(bar);"))

(deftest comma-newlines
  (fillcode-test "foo(bar,\n  baz);" "foo(bar, baz);")
  (fillcode-test "foo(bar\n  ,baz);" "foo(bar, baz);")
  (fillcode-test "foo(\n  bar,baz\n  );" "foo(bar, baz);")
  (fillcode-test "foo(\n  bar\n  ,\n  baz\n  );" "foo(bar, baz);"))

(deftest arithmetic-whitespace
  (fillcode-test "foo(bar+baz);" "foo(bar + baz);")
  (fillcode-test "foo(bar-  baz);" "foo(bar - baz);")
  (fillcode-test "foo(bar /baz);" "foo(bar / baz);")
  (fillcode-test "foo(bar  *  baz);" "foo(bar * baz);"))

(deftest blank-lines
  ; shouldn't fill across blank lines
  (fillcode-test "foo(\n\n);")
  (fillcode-test "foo(\n\n  bar, baz);")
  (fillcode-test "foo(bar\n\n  ,baz);")
  (fillcode-test "foo(bar,\n\n  baz);")
  (fillcode-test "foo(\n  bar, baz\n\n  );" "foo(bar, baz\n\n  );")

  (fillcode-test "foo(\n\n  bar, baz);" nil 9)
  (fillcode-test "foo(bar\n\n  ,baz);" nil 9)
  (fillcode-test "foo(bar,\n\n  baz);" nil 9)
  (fillcode-test "foo(\n  bar, baz\n\n  );" "foo(bar, baz\n\n  );" 9))

(deftest indentation
  ; indentation at the beginning of the line should be preserved
  (fillcode-test "foo();")
  (fillcode-test " foo();")
  (fillcode-test "  foo();")
  (fillcode-test "    foo();"))

(deftest simple-fill
  (fillcode-test "foo(bar, baz);" "
foo(bar,
    baz);" 10)

  (fillcode-test "foo(bar,baz);" "
foo(bar,
    baz);" 6)

  ; a
  (fillcode-test "foo(bar, baz, baj);" "
foo(bar,
    baz,
    baj);" 10)

  ; z
  (fillcode-test "foo(bar, baz, baj);" "
foo(bar,
    baz,
    baj);" 11)

  ; ,
  (fillcode-test "foo(bar, baz, baj);" "
foo(bar,
    baz,
    baj);" 12)

  ; [space]
  (fillcode-test "foo(bar, baz, baj);" "
foo(bar, baz,
    baj);" 13)

  ; b
  (fillcode-test "foo(bar, baz, baj);" "
foo(bar, baz,
    baj);" 14)

  ; if no fill point before fill-column, don't try to fill. wait until the
  ; next fill point.
  (fillcode-test "foo(bar, bazbaz, baj);" "
foo(bar,
    bazbaz,
    baj);" 10)
  )

(deftest multiple-identifiers-between-commas
  (fillcode-test "foo(bar baz, baj baf);" "
foo(bar baz,
    baj baf);" 18)

  (fillcode-test "foo(bar baz baj, baf bat bap);" "
foo(bar baz baj,
    baf bat bap);" 22))

(deftest nested
  (fillcode-test "foo(x(y, z));" "foo(x(y, z));")
  (fillcode-test "foo( x ( y ,z ));" "foo(x(y, z));")
  (fillcode-test "foo( x ( y,z ) ,a( b ,c ));" "foo(x(y, z), a(b, c));")

  ; in cc-mode and friends, filling at baz brings it to the same fill column as
  ; the second parenthesis, which doesn't help any. so it's not filled.
  (fillcode-test-in-mode "foo(barbar(baz));" "foo(barbar(
    baz));" 'python-mode 12)

  ; try with the fill column on different parts of the nested function call.
  ; the full text is:  foo(barbarbar, baz(x), baf)
  ;
  ; z
  (fillcode-test "foo(barbarbar, baz(x), baf);" "
foo(barbarbar,
    baz(x), baf);" 17)

  ; (
  (fillcode-test "foo(barbarbar, baz(x), baf);" "
foo(barbarbar,
    baz(x), baf);" 18)

  ; [space]
  (fillcode-test "foo(barbarbar, baz(x), baf);" "
foo(barbarbar, baz(x),
    baf);" 22)

  ; b
  (fillcode-test "foo(barbarbar, baz(x), baf);" "
foo(barbarbar, baz(x),
    baf);" 23)
)

(deftest arithmetic
  ; these are ok as is
  (fillcode-test "foo(bar + baz);" nil 16)
  (fillcode-test "foo(bar - baz);" nil 16)
  (fillcode-test "foo(bar / baz);" nil 16)
  (fillcode-test "foo(bar * baz);" nil 16)
  (fillcode-test "foo(bar == baz);" nil 16)
  (fillcode-test "foo(bar != baz);" nil 16)
  (fillcode-test "foo(bar >= baz);" nil 16)
  (fillcode-test "foo(bar <= baz);" nil 16)

  ; these should be filled
  (fillcode-test "foo(bar + baz);" "
foo(bar +
    baz);" 11)
  (fillcode-test "foo(bar - baz);" "
foo(bar -
    baz);" 11)
  (fillcode-test "foo(bar / baz);" "
foo(bar /
    baz);" 11)
  (fillcode-test "foo(bar * baz);" "
foo(bar *
    baz);" 11)
  (fillcode-test "foo(bar == baz);" "
foo(bar ==
    baz);" 11)
  (fillcode-test "foo(bar != baz);" "
foo(bar !=
    baz);" 11)
  (fillcode-test "foo(bar >= baz);" "
foo(bar >=
    baz);" 11)
  (fillcode-test "foo(bar <= baz);" "
foo(bar <=
    baz);" 11)
  (fillcode-test "foo(bar + baz - baf / baj * bap);" "
foo(bar +
    baz -
    baf /
    baj *
    bap);" 11)
  (fillcode-test "foo(bar+baz);" "
foo(bar +
    baz);" 6)

  ; the minus sign is tricky. when it's used to indicate a negative scalar, it
  ; *shouldn't* be normalized.
  (fillcode-test "foo(-bar);")
  (fillcode-test "foo(-3);")
  ; ...but the whitespace should still be normalized
  (fillcode-test "foo(bar,-baz);" "foo(bar, -baz);")
  (fillcode-test "foo(bar,-3);" "foo(bar, -3);")

  ; TODO: these aren't normalized right yet, because of the difficulties in
  ; distinguishing the subtraction operator from the minus sign, and the
  ; multiplication operator from the pointer asterisk. :/
;;   (fillcode-test "foo(bar -baz);" "foo(bar - baz);")
;;   (fillcode-test "foo(bar*baz);" "foo(bar * baz);")
  )


(deftest multiple-parenthesized-expressions
  ;; if there are multiple top-level parenthetic expressions, we should fill
  ;; all of them, not just the first
  (fillcode-test "foo(bar) foo(baz,baj);" "foo(bar) foo(baz, baj);")
  (fillcode-test "foo(bar) foo(baz,baj);" "
foo(bar) foo(baz,
             baj);" 18)

  ;; ...even if they span multiple lines. (not yet. TODO for later maybe.)
;;   (fillcode-test "if (bar) \\\n  foo(baz,baj);" "
;; if (bar) \\
;;   foo(baz,
;;       baj);" 12)
)

(deftest non-fill-points
  ;; make sure that tokens aren't normalized or filled at other special tokens
  (fillcode-test "foo(bar.baz);" nil 9)
  (fillcode-test "foo(bar_baz);" nil 9)
  (fillcode-test "foo(bar%baz);" nil 9)
  (fillcode-test "foo(bar$baz);" nil 9)
  (fillcode-test "foo(bar`baz);" nil 9)
  (fillcode-test "foo(bar@baz);" nil 9)
  (fillcode-test "foo(bar!baz);" nil 9)
  (fillcode-test "foo(bar:baz);" nil 9)
  (fillcode-test "foo(bar?baz);" nil 9)
  (fillcode-test "foo(bar->baz);" nil 9)
  (fillcode-test "foo(bar *baz);" nil 9)  ;; pointers in c++
  (fillcode-test "foo(bar* baz);" nil 9)
  (fillcode-test "foo(bar*baz);" nil 9)
  (fillcode-test-in-mode "foo(bar#baz);" nil 'java-mode 9))

(deftest literals
  ;; string literals and comments should be kept intact and treated as single,
  ;; unbreakable tokens, not normalized or filled inside
  (fillcode-test "foo(\"bar,baz\");")
  (fillcode-test-in-mode "foo(\"bar,baz\");" nil 'java-mode 20)

  (fillcode-test "foo(\"bar,baz\");" nil 9)
  (fillcode-test-in-mode "foo('bar,baz');" nil 'java-mode 9)

  (fillcode-test "foo(\"bar\" + baz + \"baj\");" "
foo(\"bar\" +
    baz +
    \"baj\");" 12)

  ;; TODO: temporarily disabled. it gets to here:
  ;;   foo("bar + bar" +
  ;;       baz + "baj + baj");
  ;; with the cursor on the semicolon, looks backward for a fill point, finds
  ;; the plus sign in the string, says nope, it's in a string, and doesn't know
  ;; how to look backward *past* the string to the other plus sign. grr!!!
;;   (fillcode-test "foo(\"bar + bar\" + baz + \"baj + baj\");" "
;; foo(\"bar + bar\" +
;;     baz +
;;     \"baj + baj\");" 16)

  ; don't fill whole-line comments (# and //)
  (fillcode-test-in-mode "foo(bar); // baz, baj" nil 'java-mode 16)

  ; emacs 21's python.el doesn't set `fill-paragraph-function', so it doesn't
  ; fill this line...but emacs 22's python.el does. i haven't yet figured out
  ; how to make this test portable. :/)
  (fillcode-test-in-mode "foo(bar) # baz, baj" nil 'python-mode 16)

  (fillcode-test-in-mode "foo(bar, /*baz ,baj*/, bax);" "foo(bar,
    /*baz ,baj*/,
    bax);" 'java-mode 6)

  (fillcode-test-in-mode "foo(bar, //baz ,baj,\nbax);" "foo(bar,
    //baz ,baj,
    bax);" 'c++-mode 6)

  (fillcode-test-in-mode "foo(bar, //baz ,baj,\nbax);" "
foo(bar, //baz ,baj,
    bax);" 'c++-mode 20)

  ; py-in-literal in python-mode.el 4.6.18.2 (the old one maintained w/python)
  ; is buggy. when it's used in the test below. it returns nil when it's inside
  ; the comment. :/
;;   (fillcode-test-in-mode "foo(bar, #baz ,baj,\nbax);" "foo(bar,
;;     #baz ,baj,
;;     bax);" 'python-mode 6)

  (fillcode-test-in-mode "foo(// bar, baz
   bajbaj, bax);" "
foo(// bar, baz
    bajbaj,
    bax);" 'java-mode 12)


  ; literals should still be normalized *around*
  ; TODO: re-enable me!
;;   (fillcode-test "foo(\"bar\",\"baz\");" "foo(\"bar\", \"baz\");")

  ; and after
  (fillcode-test-in-mode "foo(//bar\nbaz ,\nbaj);" "
foo(//bar
    baz, baj);" 'java-mode)

  ; if the first choice fill point is in a literal, fall back to second choice
  (fillcode-test-in-mode "foo(\"baz,\" + bar);" "
foo(\"baz,\" +
    bar);" 'java-mode 13)
)

; if there's a prefix argument, fill at all top-level fill points. fill at
; other fill points only as needed.
;; (deftest prefix-argument ()
;;   (fillcode-test "foo(bar);" 80 t)

;;   (fillcode-test "foo(bar,baz);" "
;; foo(bar,
;;     baz);" 80 t)

;;   (fillcode-test "foo(bar,baz);" "
;; foo(bar,
;;     baz);" 12 t)

;;   (fillcode-test "foo(bar,baz(baj));" "
;; foo(bar,
;;     baz(baj));" 80 t)

;;   (fillcode-test "foo(bar,baz(baj, bak));" "
;; foo(bar,
;;     baz(baj, bak));" 80 t)

;;   (fillcode-test "foo(baz(baj, bak), bar);" "
;; foo(baz(baj, bak),
;;     bar);" 80 t ))


;; test fillcode-beginning-of-statement and fillcode-end-of-statement with
;; the given buffer contents and mode (adding semicolons as needed). they're
;; tried with point at begin, end, and halfway between. begin and end are
;; the expected values.
(defun test-boundaries (contents begin end)
  ; try all three modes
  (dolist (mode '(python-mode c++-mode java-mode))
    ; set up the buffer
    (with-temp-buffer
      (toggle-mode-clean mode)
      (insert-string contents) ; *after* setting mode
      ; try at beginning, end, and in between
      (dolist (point (list begin end
                           (+ begin (/ (- end begin) 2))))
        (progn
          (goto-char point)
          (assert-equal begin (fillcode-beginning-of-statement))
          (assert-equal end (fillcode-end-of-statement)))))))

(deftest statement-boundaries
  ;; note that (point-min) is 1
  (test-boundaries "foo();\nbar();" 1 7)
  (test-boundaries "foo();\nbar();" 8 14)

  (test-boundaries "foo();\nbar( x );" 1 7)
  (test-boundaries "foo();\nbar( x );" 8 17)

  (test-boundaries "foo(x );\nbar(y);" 1 9)
  (test-boundaries "foo(x );\nbar(y);" 10 17)

  ; point is at the beginning of the buffer, so *only* the first statement
  ; should be filled
  (fillcode-test "foo(y);\nbar( x)" "foo(y);\nbar( x)")

  ; open parens after fill points shouldn't trip us up
  (fillcode-test "foo(x, (y));\nbar( x)" "foo(x, (y));\nbar( x)"))

(deftest subexpression-affinity
  ; don't fill inside a subexpression if it would fit on one line
  (dolist (i '(15 16 17 18))
    (fillcode-test "foo(bar, baz(a, b));" "
foo(bar,
    baz(a, b));" i))

  (fillcode-test-in-mode "foo(bar, baz(a, b));" "
foo(bar,
    baz(a, b));" 'c++-mode 19)

  (fillcode-test "foo(barbarbar, (x, y), baz);" "
foo(barbarbar,
    (x, y), baz);" 19))


(defconst ordered-fill-points
  '((",")
    (" &&" " ||")
    (" ==" " !=" " >=" " <=" " >" " <")
    (" +" " -" " /" " *")
    (" &" " |" " ~" " ^" " <<" " >>")))

(defun precedence-test-in-mode (fill-points modes)
  "Test that fillcode prefers to fill at fill points in (car fill-points) over
fill points in any of the lists in (cdr fill-points)."
  (when fill-points  ; base case
    (dolist (mode modes)
      (dolist (first (car fill-points))
        (dolist (second (apply 'append (cdr fill-points)))
          (fillcode-test-in-mode (concat "foo(bar" first "baz" second " baj);")
                                 (concat "
foo(bar" first "
    baz" second " baj);")
                                 mode 16))))
    (precedence-test (cdr fill-points))))  ; recursive step

(deftest fill-point-hierarchy
  ; test which fill points take precedence
  (precedence-test-in-mode ordered-fill-points '(python-mode))
  (precedence-test-in-mode (cons '(";") ordered-fill-points)
                           '(java-mode c++-mode)))
