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
;; 7-August-2005|0.1|~/packages/fillcode.el| 

;; This minor mode enhance the fill functions when in source code major modes,
;; including c-mode, java-mode, and python-mode. Specifically, it provides a
;; new fill function that intelligently fills some parts of source code, like
;; function calls and definitions, if the language mode's fill function
;; returns nil.
;;
;; M-x fillcode-mode toggles fillcode-mode on and off in the current buffer.
;;
;; TODO:
;; - if first arg doesn't pass fill-column, but next one does, newline first
;; - if a nested call was filled to more than one line, newline-and-indent
;; after the call's close paren and comma. ideally before call/open paren too
;; - add beginning-of-statement fns for more languages
;; - option for preferring first arg on first line or on next line
;; - fill things besides function calls, eg arithmetic expressions, string
;;   constants (language specific, ick), java throws clauses
;; - make it work in c-mode-common (since M-q gets set to c-fill-paragraph)

(require 'cl)  ; for the case macro

(define-minor-mode fillcode-mode
  "Toggle fillcode mode.
With no argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

Fillcode mode can intelligently fill some parts of source code, like function
calls and definitions, in many languages.
"
 ;; initial value
 nil
 ;; mode line indicator
 " Fillcode"
 ;; keymap
 nil
 ;; these forms run when fillcode-mode is enabled or disabled
 (make-local-variable              ;; The primary fill function. Fillcode only
  'fillcode-wrapped-fill-function) ;; runs if this returns nil.
 (make-local-variable 'fill-paragraph-function)
 (if fillcode-mode
     (setq fillcode-wrapped-fill-function fill-paragraph-function
           fill-paragraph-function 'fillcode-fill-paragraph)
   (if (eq fill-paragraph-function 'fillcode-fill-paragraph)
       (setq fill-paragraph-function fillcode-wrapped-fill-function)))
 )


(defun fillcode-fill-paragraph (&optional arg)
  "Fill code at point if fillcode-wrapped-fill-function returns nil.

If fillcode-wrapped-fill-function is nil, fills code. If it's non-nil, runs it
first, and only fills code if it returns nil.

Intended to be set as fill-paragraph-function.
"
  (save-excursion
    (if fillcode-wrapped-fill-function
        (let ((ret (fillcode-wrapped-fill-function arg)))
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
  (catch 'closeparen
    (while t
      (let ((c (char-to-string (char-after))))
;;         (edebug)
        ; normalize whitespace
        (if (equal "," c)
            (delete-horizontal-space))
        (if (or (equal (char-to-string (char-before)) ",")
                (string-match "[ \t]" c))
            (fixup-whitespace))
        ; if we hit a comma or close paren, and the next non-whitespace char
        ; is past the fill column, fill! (ie insert a newline and indent)
        (if (or (equal c ",") (equal c ")"))
            (if (>= (current-column) fill-column)
                (save-excursion
                  (skip-chars-backward "^,()")
;;                   (if (not (equal ")" (char-to-string (char-before))))
                  (newline-and-indent))))
        ; at a close paren, if the function call was filled at all, ie the
        ; close paren is on a lower line than the open paren, newline.
;;         (if (and (equal c ")"))

        ; close parenthesis is our base case; return!
        (if (equal c ")")
            (throw 'closeparen t))
        ; open parenthesis is our recursive step; recurse!
        (if (equal c "(")
            (progn (forward-char) (fillcode t)))
        ; if we hit a newline, delete it, otherwise advance
        (if (eolp)
            (delete-indentation t)
          (forward-char))
        ))))


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

  (search-forward "(" (line-end-position))
  )

