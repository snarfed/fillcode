;;; fillcode.el --- Fillcode minor mode
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
;;
;; This minor mode enhance the fill functions when in source code major modes,
;; such as c-mode, java-mode, and python-mode. Specifically, it provides a new
;; fill function that intelligently fills some parts of source code, like
;; function calls and definitions, if the language mode's fill function
;; doesn't already.
;;
;; M-x fillcode-mode toggles fillcode-mode on and off in the current buffer.
;;
;; TODO:
;; - fillcode still fills previous statement in cc-mode multi-line comments
;; - python (and other) comments are filled *out of the comment* if they pass
;;   the fill column. e.g.:
;;   foo(bar,  # i like bar
;;       baz)
;; - fill expressions outside parentheses
;; - somehow include the assignment = operator and <, > in
;;   fillcode-fill-point-re. (how to handle <, > with e.g. templates?!?)
;; - add beginning-of-statement fns for more languages
;; - make it compatible with filladapt-mode
;; - handle c++ and python comments better. (the line after them doesn't get
;;   indented.)
;; - the first prototype in a c++ class definition after an access specifier,
;;   e.g. public:, doesn't get filled

(defconst fillcode-version "0.5")

(require 'cl)  ; for the case macro

(require 'cc-bytecomp)  ; for c-in-literal and c-literal-limits
(cc-require 'cc-engine)

; gnu emacs supports optional forms as the last arguments to
; define-minor-mode; they're evaluated when the minor mode is enabled or
; disabled. this is really nice, but xemacs' define-minor-mode doesn't have
; it, so i have to advise the fillcode-mode function instead (below).
(define-minor-mode fillcode-mode
  "Toggle fillcode mode.
With no argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

Fillcode mode can intelligently fill some parts of source code, like function
calls and definitions, in many languages.

To see what version of fillcode you are running, enter `\\[fillcode-version]'.

For more information, see http://snarfed.org/space/fillcode"
 nil         ;; initial value
 " Fillcode" ;; mode line indicator
 nil)        ;; keymap


(defun fillcode-version ()
  "Echo the current version of fillcode mode in the minibuffer."
  (interactive)
  (message "Using fillcode mode version %s" fillcode-version))

(defadvice fillcode-mode (after fillcode-mode-setup-and-teardown)
 ;; run these when fillcode-mode is enabled or disabled. the fillcode-mode var
 ;; is set before these run.
 (make-local-variable              ;; The primary fill function. Fillcode only
  'fillcode-wrapped-fill-function) ;; runs if this returns nil.
 (make-local-variable 'fill-paragraph-function)

 (if fillcode-mode
     ; this runs when fillcode is enabled...
     (progn 
       (if (not (eq fill-paragraph-function 'fillcode-fill-paragraph))
           (setq fillcode-wrapped-fill-function fill-paragraph-function)
         (setq fillcode-wrapped-fill-function nil))
       (setq fill-paragraph-function 'fillcode-fill-paragraph)
       (ad-activate 'c-fill-paragraph))

   ; ...and this runs when it's disabled.
   (progn
     (if (eq fill-paragraph-function 'fillcode-fill-paragraph)
         (setq fill-paragraph-function fillcode-wrapped-fill-function))
     (ad-deactivate 'c-fill-paragraph))))

(ad-activate 'fillcode-mode)


(defadvice c-fill-paragraph (around fillcode-if-in-code)
  "If in fillcode-mode, fill code when in `cc-mode'.

`cc-mode' replaces `fill-paragraph' with its own function, `c-fill-paragraph',
which only calls fill-paragraph if it's inside a comment or string literal, and
narrows to that comment or string literal. Fillcode operates on code itself, so
it needs a chance to run (without narrowing!), which this advice provides."
 (when fillcode-mode
   (let ((fill-paragraph-function nil))
     ad-do-it)
   (fillcode-fill-paragraph arg))) ; arg is c-fill-paragraph's arg



(defgroup fillcode nil
  "Fill code"
  :group 'fill)

(defcustom fillcode-fill-points
  (list
   ";[^;]"
   ",[^,]"
   "&&[^&]\\|||[^|]"                    ; boolean operators
   "<[^<=]\\|[^-]>[^>=]\\|[<>!=]=[^=]"  ; comparators. > is a fill pt, -> isn't
   (concat "/[^=]\\|+[^+=]\\|"          ; arithmetic operators
           ; asterisks are used as pointers in c and c++, so to be
           ; conservative, they're only fill points if they're surrounded by
           ; whitespace. yes, this means that expressions like foo = bar*baz;
           ; won't be normalized or filled correctly.
           "\\s-\\*\\s-\\|"
           ; minus signs are only fill points if they're not being used as a
           ; negative sign. approximate this by checking if they're followed
           ; by whitespace. (it's a very bad approximation.)
           "-\\s-")
   ;; TODO: iostreams insertion operators <<, >>
   "[&|~^][^&|=]\\|<<[^<]\\|>>[^>]"    ; bitwise operators
   "[({[][^({[]")

  "A list of regular expressions used to find fill points.
A fill point is a point in an expression where a newline can reasonably be
inserted. This list contains regular expressions that identify fill points.

The list is ordered by precedence. The first regexp contains fill points that
fillcode prefers to fill at first, if possible. If none of them are found,
fillcode tries the next regexp, and so on.

Each regexp match must include one character *after* the fill point ends.

You may modify this to allow fillcode to handle new languages.

Note that the single = (assignment) operator and < and > operators are
unfortunately absent."
  :type 'string
  :group 'fillcode)

(defun fillcode-fill-point-re ()
  "Build a fill point regexp from the user-customizable variable
`fillcode-fill-points`. A function, not a variable, so that it won't skew if
the user changes `fillcode-fill-points`."
  (mapconcat 'identity fillcode-fill-points "\\|"))

(defun fillcode-fill-paragraph (arg &optional arg2 arg3 arg4)
  "Fill code at point if `fillcode-wrapped-fill-function' is nil.

If `fillcode-wrapped-fill-function' is nil, fills code. If it's
non-nil, runs it first, and only fills code if it returns nil.

Intended to be set as `fill-paragraph-function'."
  ; first, consider calling the wrapped fill function
  (let ((ret
         (cond
          ; if we're in cc-mode, this was called by the `c-fill-paragraph'
          ; advice. so, don't call it again, it'd recurse infinitely.
          ((eq fillcode-wrapped-fill-function 'c-fill-paragraph)
           nil)
          ; `python-fill-paragraph' in CVS Emacs' python.el always returns
          ; t (grr!), so instead of looking at its return value, we fill if
          ; the end of the line is not in a comment or string literal
          ((and (eq major-mode 'python-mode)
                (not (save-excursion (end-of-line) (fillcode-in-literal))))
           nil)
          ; otherwise, if it's set, call the wrapped fill function
          (fillcode-wrapped-fill-function
           (funcall fillcode-wrapped-fill-function arg)))))


    ; if the wrapped fill function did something, or we're possibly in a
    ; multi-line literal, don't do anything more
    (if (or ret
            (member (fillcode-in-literal) '(c c++ comment)))
        ret
      ; otherwise, normalize whitespace and fill
      (save-excursion (save-restriction
        (narrow-to-region (fillcode-beginning-of-statement)
                          (fillcode-end-of-statement))
        (fillcode-normalize-whitespace)
        (goto-char (point-min))
        (condition-case nil  ; fill until we hit the end of the statement
            (while (< (point) (point-max))
              (with-syntax-table c-mode-syntax-table
                (fillcode arg)))
            (end-of-buffer t))
          t)))))



(defun fillcode (&optional arg)
  "Fill code at point.
The actual function-call-filling algorithm. Fills function calls and prototypes
if it thinks the point is on a statement that has one.

Returns t if it actually filled somewhere (not including just normalizing
whitespace), nil otherwise."
  ; the main loop. advances through the statement, filling as necessary.
  ; recursive so we can easily determine, after we've finished with a
  ; subexpression, whether we filled inside it.
  (let ((filled nil))
    (catch 'sexp-end
      (while (fillcode-forward)
;;         (edebug)
        ; skip literals
        (while (fillcode-in-literal)
          (forward-char))

        ; fill if we need to
        (when (fillcode-should-fill)
          (fillcode-fill-at-fill-point 'backward)
          (setq filled t))

        ; end of a sexp. return!
        (when (and (char-after)
                 (eq (char-syntax (char-after)) ?\)))
            (throw 'sexp-end t))

        ; if a sexp extends beyond fill-column, and there's an earlier
        ; *non-open-paren* fill point we can use, fill at that fill point
        (when (and (char-after)
                   (eq (char-syntax (char-after)) ?\()
                   (save-excursion
                     (fillcode-find-fill-point-backward)
                     (not (eq (char-syntax (char-before)) ?\())))
          (if (< fill-column (fillcode-fill-point-column-after-sexp))
              (fillcode-fill-at-fill-point 'backward))
          (forward-char)
          (if (fillcode arg)
              (fillcode-fill-at-fill-point 'forward)))))

    ; return t if we filled, nil otherwise
    filled))

(defun fillcode-fill-at-fill-point (direction)
  "Fill at the nearest fill point.
Nearest fill point is found either before or after point, depending on
whether direction is 'backward or 'forward, respectively.

Moves point to the first non-whitespace character on the line after the fill.

If filling brings the new line to the same point as it was on the previous
line, doesn't fill and leaves point where it was before."
  (catch 'filled
    (let ((orig-pt (point))
          (find-fn (if (eq direction 'forward)
                       'fillcode-find-fill-point-forward
                     'fillcode-find-fill-point-backward)))

    (if (funcall find-fn)
      ; found a fill point
      (progn
        (let ((orig-col (current-column)))
          (insert "\n")
          (indent-according-to-mode)
          (when (>= (current-column) orig-col)
            ; no good, we're at the same column as before we filled. ok
            ; then, just indent a little past the last line instead.
            (indent-line-to (+ (fillcode-get-last-line-indent-offset)
                               (fillcode-get-mode-indent-offset))))))

      ; no usable fill point found
      (goto-char orig-pt)))))

(defun fillcode-forward ()
  "Move forward to the next 'interesting' character. (Word-constituent
characters (letters, numbers, underscores, etc.) and whitespace are not
interesting.) Uses the current syntax table and `skip-syntax-forward'.

If point is already on an interesting character, more forward just one
character.

Return t if it moved point at all, nil otherwise."
  (unless (eolp)
    (if (eq (skip-syntax-forward "w_ ") 0)
        (forward-char))
    t))

(defun fillcode-forward-sexp ()
  "Call forward-sexp and catch any errors.
Return t if it moved point at all, nil otherwise."
  (unless (eolp)
    (condition-case nil (forward-sexp)
      (scan-error
       (forward-char)
       t))))

(defun fillcode-beginning-of-statement ()
  "Return the start position of the statement that point is currently in. Uses
the major mode's beginning-of-statement function, if it has one. Otherwise, for
safety, just uses the beginning of the line."
  (case major-mode
    ((c-mode c++-mode java-mode objc-mode perl-mode)
     ; if we're at the beginning of the statement, `c-beginning-of-statement'
     ; will go to the *previous* statement. so, first move past a
     ; non-whitespace character.
     (beginning-of-line)
     (re-search-forward "\\S-" nil t)  ; whitespace
     (c-beginning-of-statement)
     ; NB: use point-at-bol for xemacs compatibility. the emacs function is
     ; line-beginning-position; point-at-bol is just an alias. xemacs, however,
     ; only has point-at-bol. (same with point-at-eol/line-end-position.)
     (point-at-bol))

    ((python-mode)
     (save-excursion
       (if (functionp 'py-goto-statement-at-or-above)
           (py-goto-statement-at-or-above)
         (python-beginning-of-statement))
       (point)))

    ; `c-beginning-of-statement' might be a good fallback for unknown
    ; languages, but it occasionally fails badly, e.g. in `perl-mode'.
    (otherwise
     (point-at-bol))))  ; default


(defun fillcode-end-of-statement ()
  "Return the end position of the statement that point is currently in.
Uses the major mode's end-of-statement function, if it has one. Otherwise,
for safety, just uses the end of the line."
  (save-excursion
    (case major-mode
      ((c-mode c++-mode java-mode objc-mode perl-mode)
       ; c-end-of-statement does the right thing with if conditions, for
       ; statements, {...} blocks, and statements that end with semicolon.
       (c-end-of-statement))

      ((python-mode)
         (let ((start (point)))
           (if (if (functionp 'py-goto-statement-below)
                   (py-goto-statement-below)
                 (python-next-statement))
               (search-backward ")" start 'p)
             (condition-case nil (forward-char) (error nil))))))
  
      ;`c-end-of-statement' might be a good fallback for unknown languages,
      ; but it occasionally fails badly, e.g. in `perl-mode'.
    (point-at-eol)))


(defun fillcode-normalize-whitespace ()
  "Normalize the current statement's whitespace, starting at point.
Specifically, no newlines, spaces before commas or open parens or after
close parens, one space after commas, one space before and after arithmetic
operators. Except string literals and comments, they're left untouched.

Uses `fillcode-collapse-whitespace-forward'."
    ; don't fill across blank lines, whether they're before point...
    (save-excursion
      (forward-line)
      (beginning-of-line)
      (if (re-search-backward "\n\\s-*\n" nil t)
          (narrow-to-region (match-end 0) (point-max))))
    ; ...or after
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (if (re-search-forward "\n\\s-*\n" nil t)
          (narrow-to-region (point-min) (match-beginning 0))))

    (goto-char (point-min))
    ; preserve indentation. use point-at-eol for xemacs compatibility.
    (if (re-search-forward "\\S-" (point-at-eol) t)
        (backward-char))
    (while (not (eobp))
      (fillcode-collapse-whitespace-forward)))

(defun fillcode-collapse-whitespace-forward ()
  "Delete newlines, normalize whitespace, and/or move forward one character.
Specifically, no spaces before commas or open parens or after close parens,
one space after commas, one space before and after arithmetic operators.
Except string literals and comments, they're left untouched. Then advance
point to next non-whitespace char."
;;     (edebug)
  (cond
   ; if we're in a string literal or comment, skip to the end of it 
   ((fillcode-in-literal)
    ; TODO: maybe goto-char (cdr c-literal-limits) here would be faster?
    (forward-char)
    (if (equal "\n" (char-to-string (char-before)))
        (indent-according-to-mode)))

   ; if we're at the end of the line, pull up the next line
   ((eolp)
    (delete-indentation t))
 
   ; if we're on whitespace, delete it. if that brings us to a fill point,
   ; fall down to the logic below. otherwise, normalize to exactly one space
   ; and continue.
   ((looking-at "\\s-")
    (delete-horizontal-space)
    (when (and (not (looking-at (fillcode-fill-point-re)))
               (not (looking-at "[([{]")))
      (fixup-whitespace)
      (if (looking-at "\\s-")  ; (*not* including newlines)
          (forward-char))))

   ; if we're before a non-comma/semicolon/open paren fill point, add a space
   ((and (looking-at (fillcode-fill-point-re))
         (not (looking-at "[,;([{]")))
    (insert " ") 
    (goto-char (match-end 0)))

   ; if we're after a fill point, insert a space. (note that the fill point
   ; regexp ends at the first char *after* the operator.)
   ((and (save-excursion
           (progn 
             (condition-case nil (forward-char) (error nil))
             ; use point-at-bol for xemacs compatibility
             (re-search-backward (fillcode-fill-point-re) (point-at-bol) t)))
         (equal (point) (1- (match-end 0)))
         (not (save-excursion (backward-char) (fillcode-in-literal))))
    (fixup-whitespace)
    ; skip *past* the char we were on originally. if we inserted a
    ; space, that's two chars forward, otherwise just one.
    (forward-char (if (looking-at " ") 2 1)))

   ; ...otherwise, base case: advance one char
   (t (forward-char))))


(defun fillcode-should-fill ()
  "Return t if we should fill at the last fill point, nil otherwise.

We should fill if:

- there's a fill point on this line, AND
- we're not in a comment or string literal, AND
- the current char is at or beyond `fill-column'"
  (and
   (not (fillcode-in-literal))               ; not in a literal?
   (>= (current-column) fill-column)         ; past fill-column?
   (save-excursion
     (fillcode-find-fill-point-backward))))  ; fill point on this line?


(defun fillcode-find-fill-point-forward (&optional bound)
  ; use point-at-eol for xemacs compatibility
  (fillcode-find-fill-point-helper 're-search-forward
                                   (if bound bound (point-at-eol))))

(defun fillcode-find-fill-point-backward (&optional bound)
  ; the fill point regexp ends at the first char *after* the
  ; operator...so, move forward one char before searching.
  (forward-char)
  ; use point-at-bol for xemacs compatibility
  (fillcode-find-fill-point-helper 're-search-backward
                                   (if bound bound (point-at-bol))))

(defun fillcode-fill-point-column-after-sexp ()
  "Return the column of the closest fill point after the sexp at point."
  (save-excursion
    (fillcode-forward-sexp)
    (if (not (fillcode-find-fill-point-forward))
        (end-of-line))
    (current-column)))


(defun fillcode-find-fill-point-helper (re-search-fn bound)
  "Move to the best fill point to fill at on the current line.

Fill points are defined by `fillcode-fill-points'; commas, open parens,
arithmetic operators, ||s, &&s, etc. This function finds the closest one either
before or after point, depending on `forward'.

It searches for fill points in the order that their regexps are specified in
`fillcode-fill-points'.

Returns t if it found a fill point, nil otherwise."
  (when
   (catch 'found
     (dolist (re fillcode-fill-points)
       (when (and (funcall re-search-fn re bound t)
                  (save-match-data
                    ; can't fill if we're in or immediately after a literal
                    (not (fillcode-in-literal))
                    (not (save-excursion (backward-char)
                                         (fillcode-in-literal)))))
         (throw 'found t))))

    ; found a fill point
    (goto-char (1- (match-end 0)))))


(defun fillcode-in-literal ()
  "Return non-nil if inside a comment or string literal, nil otherwise.
Determines whether point is inside a comment, string literal, or other segment
that shouldn't be normalized or filled. Piggybacks on the major modes, since
it will usually have its code for this.

Unfortunately, the major modes' in-literal functions (e.g. `c-in-literal' do
*not* consider literals' start tokens (\", ', /*, //, #) to be part of the
literal, so they return nil if point is on the start token. We want them to
return non-nil if we're past the first char of the start token, so
`fillcode-in-literal' returns non-nil instead."
  (let ((in-literal-fn
         (case major-mode
           ((python-mode) (if (functionp 'py-in-literal)
                              'py-in-literal 'python-in-string/comment))
           (otherwise 'c-in-literal))))

    ; if the major mode says point *or* the char *after* point is in a literal,
    ; or if two chars after point is a comment, then we're in a literal.
    (or (funcall in-literal-fn)
        (condition-case nil
            (save-excursion
              (forward-char)
              (or (funcall in-literal-fn)
                  (progn
                    (forward-char)
                    (member (funcall in-literal-fn) '(c c++)))))
          (error nil)))))

(defun fillcode-get-mode-indent-offset ()
  "Returns the indent offset, ie the number of columns to indent, in the
current mode."
  (case major-mode
    ((python-mode) py-indent-offset)
    (otherwise c-basic-offset)))

(defun fillcode-get-last-line-indent-offset ()
  "Returns the indent offset, ie the column of the first non-whitespace
character, of the current line."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (skip-chars-forward " \t")  ; skip whitespace
    (current-column)))

(provide 'fillcode)

;;; fillcode.el ends here
