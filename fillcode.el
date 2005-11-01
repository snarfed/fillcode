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

;; LCD Archive Entry: 
;; fillcode|Ryan Barrett|fillcode@ryanb.org| 
;; Minor mode to fill function calls and other parts of source code|
;; 7-September-2005|0.1|~/packages/fillcode.el| 

;; This minor mode enhance the fill functions when in source code major modes,
;; such as c-mode, java-mode, and python-mode. Specifically, it provides a new
;; fill function that intelligently fills some parts of source code, like
;; function calls and definitions, if the language mode's fill function
;; doesn't already.
;;
;; M-x fillcode-mode toggles fillcode-mode on and off in the current buffer.
;;
;; TODO:
;; - if fill-column falls on comma after close paren of nested call, we go back
;;   inside the nested call to fill, but we don't think we're inside it in the
;;   call stack. grr!
;; - if first arg doesn't pass fill-column, but next one does, newline first
;;   (maybe option for preferring first arg on first line or on next line?)
;; - add beginning-of-statement fns for more languages
;; - fill things besides function calls, eg arithmetic expressions, string
;;   constants (language specific, ick), java throws clauses
;; - make it work in c-mode-common (since M-q gets set to c-fill-paragraph)
;; - make it compatible with filladapt-mode

(require 'cl)  ; for the case macro

(defvar fillcode-version "0.1")

(define-minor-mode fillcode-mode
  "Toggle fillcode mode.
With no argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

Fillcode mode can intelligently fill some parts of source code, like function
calls and definitions, in many languages.

For more information, see http://snarfed.org/space/fillcode
"
 ;; initial value
 nil
 ;; mode line indicator
 " Fillcode"
 ;; keymap
 nil
 ;; these forms run when fillcode-mode is enabled or disabled. the
 ;; fillcode-mode var is set before these forms run.
 (make-local-variable              ;; The primary fill function. Fillcode only
  'fillcode-wrapped-fill-function) ;; runs if this returns nil.
 (make-local-variable 'fill-paragraph-function)
 (if fillcode-mode
     (progn 
       (if (not (eq fill-paragraph-function 'fillcode-fill-paragraph))
           (setq fillcode-wrapped-fill-function fill-paragraph-function)
         (setq fillcode-wrapped-fill-function nil))
       (setq fill-paragraph-function 'fillcode-fill-paragraph))
   (if (eq fill-paragraph-function 'fillcode-fill-paragraph)
       (setq fill-paragraph-function fillcode-wrapped-fill-function)))
 )

(defgroup fillcode nil
  "Fill code"
  :group 'fill)

(defcustom fillcode-nested-calls-are-sticky nil
  "If non-nil, fillcode-mode will not separate a nested function call from its
first argument. For example, if this variable is non-nil, this:

foo(bar, baz(baj))

will fill to:

foo(bar,
    baz(baj))

If this variable is nil, it will fill to:

foo(bar, baz(
    baj))
"
  :type 'boolean
  :group 'fillcode)


(defun fillcode-fill-paragraph (arg &optional arg2 arg3 arg4)
  "Fill code at point if fillcode-wrapped-fill-function returns nil.

If fillcode-wrapped-fill-function is nil, fills code. If it's non-nil, runs it
first, and only fills code if it returns nil.

Intended to be set as fill-paragraph-function.
"
  (save-excursion
    (if fillcode-wrapped-fill-function
        (let ((ret (apply fillcode-wrapped-fill-function arg)))
          (if ret
              ret
            (fillcode)))
      (fillcode))
    ))



(defun fillcode (&optional arg)
  "Fill code at point.
The actual function-call-filling algorithm. Fills function calls and prototypes
if it thinks the point is on statement that has one.

Without arg, starts at the beginning of the statement. With arg, fills
recursively.
"
  (interactive)
  (if (not arg)
      (if (not (fillcode-beginning-of-statement))
          (error "No function found to fill")))
  (collapse-whitespace-forward)

  ; the main loop. advances through the statement, normalizing whitespace and
  ; deleting newlines along the way. the main loop should run once once and
  ; only once for each printable character. when we hit the fill-column, fill
  ; intelligently.
  (catch 'closeparen
    (while (char-after)
      (let ((c (char-to-string (char-after))))
;;         (edebug)
        ; if we're past the fill column, fill!
        (if (or (>= (current-column) fill-column)
                (and arg
                     (= (current-column) (- fill-column 1))
                     (equal c ")")
                     (equal (char-to-string (char-after (+ (point) 1))) ",")))
           (progn
              (skip-chars-backward "^(),")
              (if (string-match "[(),]" (char-to-string (char-after)))
                  (re-search-backward "[^(),]"))
               ; if sticky, don't fill the first arg of nested fn calls
              (if (and fillcode-nested-calls-are-sticky
                       arg (equal (char-to-string (char-before)) "("))
                  (save-excursion
                    (backward-char)
                    (skip-chars-backward "^(),")
                    (newline-and-indent)
                    )
                (newline-and-indent))))
        ; open parenthesis is our recursive step; recurse!
        (if (equal c "(")
            (fillcode t))
        ; close parenthesis is our base case; return!
        (if (equal c ")")
            (throw 'closeparen t))
        ; next!
        (collapse-whitespace-forward)
        )))

  ; if this is a nested function call, and we filled, newline after next comma
  (if (and arg
           (save-excursion
             (skip-chars-backward "^(" (line-beginning-position))
             (eq (point) (line-beginning-position))))
      (progn
        (collapse-whitespace-forward)  ; move past close paren
        (if (equal "," (char-to-string (char-after)))
            (progn
              (delete-horizontal-space)
              (forward-char)  ; move past comma
              (collapse-whitespace-forward)
              (newline-and-indent)))))

  ; return t to indicate that we filled something
  t
)


(defun fillcode-beginning-of-statement ()
  "Find the beginning of the statement that point is currently in.
Calls the major mode's beginning-of-statement function, if it has one.
Otherwise, for safety, just goes to the beginning of the line.

c-beginning-of-statement might be a good fallback for unknown languages, but it
occasionally fails badly, e.g. in perl-mode in some cases.
"
  (case major-mode
    ('c-mode 'c++-mode 'java-mode 'objc-mode
      (c-beginning-of-statement))
    ('python-mode
      (py-goto-statement-at-or-above))
    ('perl-mode
      (c-beginning-of-statement))
    (otherwise
      (beginning-of-line)))  ; default

  (search-forward "(" (line-end-position) t)
  )


(defun collapse-whitespace-forward ()
  "Delete newlines and normalize whitespace (no spaces before commas or open
parens or after close parens, one space after commas). Then advance point to
next non-whitespace char.
"
  (interactive)

  ; if we're on whitespace, delete and normalize it...
  (if (string-match "[(), \t\n]" (char-to-string (char-after)))
      (progn
        (delete-horizontal-space)

        ; if newline, delete and recurse
        (if (eolp)
            (progn
              (delete-indentation t)
              (collapse-whitespace-forward))
          (progn
            (if (not (string-match "[(), \n]" (char-to-string (char-after))))
                (fixup-whitespace))
            (if (string-match "[() ,]" (char-to-string (char-after)))
                (forward-char)))))

    ; else if we're after a comma, normalize to one space
    (if (equal "," (char-to-string (char-before)))
        (fixup-whitespace)

      ; ...otherwise, base case: advance one char
      (forward-char)))
    )

(provide 'fillcode)
