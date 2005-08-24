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

;; These functions enhance the behavior of Emacs' auto-fill-mode when in source
;; code major modes, including c-mode, java-mode, and python-mode.
;;
;; ...
;;
;; Since this package replaces existing Emacs functions, it cannot be
;; autoloaded. Save this in a file named fillcode.el in a Lisp directory that
;; Emacs knows about and put
;;
;;    (require 'fillcode)
;;
;; in your .emacs file.
;;
;; Note that in this release fillcode-mode is a minor mode which is off by
;; default. To turn it on by default, use
;;
;;   (setq-default fillcode-mode t)
;;
;; M-x fillcode-mode toggles fillcode-mode on/off in the current buffer.
;;
;; TODO:
;; - find the original open paren in a language-independent way
;; - remove whitespace preceding a comma
;; - option for preferring first arg on first line or on next line
;; - fill things besides function calls, eg arithmetic expressions, string
;;   constants (language specific, ick), java throws clauses
;; - ooh...make a way to add language-specific filling rules

(defun fillcode ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    ; start at the first an open parenthesis
    (if (search-forward "(" (line-end-position) t)
        (fillcode-recursive)) ; (current-column)
    ))

(defun fillcode-recursive ()
  (catch 'closeparen
    (while t
      (let ((c (char-to-string (char-after))))
        (edebug)
        ; if we hit a comma or close paren, and the next non-whitespace char
        ; is past the fill column, fill! (ie insert a newline and indent)
        (if (or (equal c ",") (equal c ")"))
            (if (>= (current-column) fill-column)
                (save-excursion
                  (skip-chars-backward "^,()")
                  (if (not (equal ")" (char-to-string (char-before))))
                      (newline-and-indent)))))
        ; close parenthesis is our base case; return!
        (if (equal c ")")
            (throw 'closeparen t))
        ; open parenthesis is our recursive step; recurse!
        (if (equal c "(")
            (progn (forward-char) (fillcode-recursive)))
        ; normalize whitespace
        (if (or (equal (char-to-string (char-before)) ",")
                (string-match " \t" c))
            (fixup-whitespace))
        ; if we hit a newline, delete it, otherwise advance
        (if (eolp)
            (delete-indentation t)
            (forward-char))
        ))))

(global-set-key [(control f11)] 'fillcode)

