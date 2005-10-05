;; elunit.el
;;
;; Copyright (C) 1903 greentea@fa2.so-net.ne.jp
;;
;; For unit testing.

(defconst elunit-version '("$Revision: 3.53 $"))

;; (setq debug-on-error t)                    ; display *Backtrace* for error.

(defvar elunit-tests nil)

(defconst elunit-error-conditions
  '(elunit-failure elunit-assertion-failure elunit-comparison-failure
                   elunit-difference-failure))

(put 'elunit-assertion-failure 'error-conditions '(elunit-assertion-failure elunit-failure error))
(put 'elunit-assertion-failure 'error-message "Assertion failure")

(put 'elunit-comparison-failure 'error-conditions '(elunit-comparison-failure elunit-failure error))
(put 'elunit-comparison-failure 'error-message "Comparison failure")

(put 'elunit-difference-failure 'error-conditions '(elunit-difference-failure elunit-failure error))
(put 'elunit-difference-failure 'error-message "Difference failure")

(defconst elunit-assertion-funcs
  '(fail assert-true assert-nil assert-stringp assert-numberp assert-atom assert-consp assert-string= assert-equal assert= assert-match assert-not-match assert-match-equal assert-match-whole assert-posix-match assert-not-posix-match assert-posix-match-equal assert-posix-match-whole))

(defun -elunit-make-assertion-func-regexp ()
  (let ((i 0)
        regexp)
    (while (< i (length elunit-assertion-funcs))
      (let ((each (regexp-quote (symbol-name (nth i elunit-assertion-funcs)))))
        (setq regexp (if regexp
                         (concat regexp "\\|" each)
                       each)))
      (setq i (1+ i)))
    (concat "(\\(" regexp "\\)\\([) \t\n]\\)")))

(defconst elunit-assertions-regexp (-elunit-make-assertion-func-regexp))

(defmacro deftest (testee &rest body)
  `(push (cons ',testee (lambda () ,@body)) elunit-tests))

(defun elunit-run (files)
  (setq elunit-tests nil)  ; clear stored tests and result buffer.
  (set-buffer (get-buffer-create "*Elunit Result*"))
  (erase-buffer)

  (-elunit-load-test-suite files)
  (set-buffer "*Elunit Result*")
  (if (null elunit-tests)
      (insert "No tests found.")
    (apply '-elunit-insert-result (-elunit-run-test (reverse elunit-tests))))
  (goto-char (point-min))
  (-elunit-show-result))

(defun -elunit-load-test-suite (files)
  (dolist (each-file files)
    (with-temp-buffer
      (insert-file-contents each-file)
      (-elunit-load-test-case each-file (buffer-string))))
  (kill-buffer "*Elunit Tmp*"))

(defun -elunit-load-test-case (filename text)
  (set-buffer (get-buffer-create "*Elunit Tmp*"))
  (insert text)
  (-elunit-embed-line-numbers filename)
  (eval-buffer)
  (erase-buffer))

(defun -elunit-embed-line-numbers (filename)
  (goto-char (point-min))
  (let (match)
    (while (re-search-forward elunit-assertions-regexp nil t)
      (replace-match (concat "(\\1 " (-elunit-lineno filename) "\\2")))))

(defun -elunit-lineno (filename)
  (format "\"%s:%d\"" filename (count-lines (point-min) (point))))

(defun -elunit-run-test (tests)
  (let ((nsuccess 0)
        (nfailure 0)
        (nerror 0))
    (dolist (each tests)
      (condition-case err
          (progn
            (funcall (cdr each))
            (insert ".")
            (setq nsuccess (1+ nsuccess)))
        (error
         (if (member (car err) elunit-error-conditions)
             (progn
               (insert "F")
               (setq nfailure (1+ nfailure)))
           (progn
             (insert "E")
             (setq nerror (1+ nerror))))
         (-elunit-insert-failure-message (car each) err))))
    (list nsuccess nfailure nerror)))

(defun -elunit-insert-failure-message (testname err)
  (let ((lineno (-elunit-extract-lineno err))
        (msg (-elunit-extract-message err)))
    (with-current-buffer (get-buffer-create "*Elunit Result*")
      (insert (format "%s:%s:%s"
                      lineno
                      (error-message-string err)
                      (symbol-name testname)))
      (insert msg)
      (insert "\n"))))

(defun -elunit-extract-lineno (err)
  (if (cdr err)
      (car (-elunit-split-signal-message err))
    ""))

(defun -elunit-split-signal-message (err)
  (if (and (cdr err)
           (stringp (cdr err)))
      (split-string (cdr err) "")
    '("") ;; it does not elunit's error.
    ))

(defun -elunit-extract-message (err)
  (let ((lst (-elunit-split-signal-message err)))
    (if (= (length lst) 2)
        (concat ": " (nth 1 lst))
      "")))

(defun -elunit-insert-result (nsuccess nfailure nerror)
  (if (or (> nfailure 0) (> nerror 0))
      (insert "\n\nFAILURE!!!"))
  (insert (format "\nTests run: %d," nsuccess))
  (insert (format " Failures: %d," nfailure))
  (insert (format " Errors: %d\n" nerror)))

(defun -elunit-show-result ()
  (display-buffer "*Elunit Result*"))

(defun fail (lineno &optional message)
  (assert-true lineno nil message))

(defun assert-true (lineno condition &optional message)
  (if (not condition)
      (signal 'elunit-assertion-failure
              (-elunit-lineno-message lineno message))))

(defun -elunit-lineno-message (lineno message)
  (format "%s%s" lineno (if message message "")))

(defun assert-nil (lineno condition &optional message)
  (-elunit-assert-nil condition (-elunit-lineno-message lineno message)))

(defun -elunit-assert-nil (condition &optional message)
  (if condition
      (signal 'elunit-assertion-failure message)))

(defun assert-stringp (lineno obj &optional message)
  (elunit-assert obj (-elunit-lineno-message lineno message) :test 'stringp))

(defun elunit-assert (condition message &key test)
  (if (not (funcall test condition))
      (signal 'elunit-assertion-failure message)))

(defun assert-numberp (lineno obj &optional message)
  (elunit-assert obj (-elunit-lineno-message lineno message) :test 'numberp))

(defun assert-atom (lineno obj &optional message)
  (elunit-assert obj (-elunit-lineno-message lineno message) :test 'atom))

(defun assert-consp (lineno obj &optional message)
  (elunit-assert obj (-elunit-lineno-message lineno message) :test 'consp))

(defun assert-string= (lineno expected actual &optional message)
  (let ((msg (-elunit-lineno-message lineno message)))
    (if (not (stringp actual))
        (signal 'elunit-comparison-failure
                (-elunit-comparison-failure-message expected
                                                    (prin1-to-string actual)
                                                    msg))
      (elunit-compare expected actual msg :test 'string=))))

(defun elunit-compare (expected actual message &key test)
  (if (not (funcall test expected actual))
      (signal 'elunit-comparison-failure (-elunit-comparison-failure-message expected actual message))))

(defun -elunit-comparison-failure-message (expected actual message)
  (format "%s expected:<%s> but was:<%s>"
          message
          (prin1-to-string expected)
          (prin1-to-string actual)))

(defun assert-equal (lineno expected actual &optional message)
  (elunit-compare expected actual (-elunit-lineno-message lineno message)
                  :test 'equal))

(defun assert= (lineno expected actual &optional message)
  (elunit-compare expected actual (-elunit-lineno-message lineno message)
                  :test '=))

(defun assert-match (lineno regexp str &optional message)
  (elunit-compare regexp str (-elunit-lineno-message lineno message)
                  :test 'string-match))

(defun assert-not-match (lineno regexp str &optional message)
  (elunit-differ regexp str (-elunit-lineno-message lineno message)
                 :test 'string-match))

(defun elunit-differ (expected actual message &key test)
  (if (funcall test expected actual)
      (signal 'elunit-difference-failure (-elunit-difference-failure-message expected actual message))))

(defun -elunit-difference-failure-message (expected actual message)
  (format "%s expected:<%s> does not differ from <%s>"
          message
          (prin1-to-string expected)
          (prin1-to-string actual)))

(defun assert-match-equal (lineno regexp str expected &optional message)
  (assert-match lineno regexp str message)
  (assert-string= lineno expected
                  (substring str (match-beginning 0) (match-end 0)) message))

(defun assert-match-whole (lineno regexp str &optional message)
  (assert-match lineno regexp str message)
  (assert-string= lineno str (substring str (match-beginning 0) (match-end 0))
                  message))

(defun assert-posix-match (lineno regexp str &optional message)
  (elunit-compare regexp str (-elunit-lineno-message lineno message)
                  :test 'posix-string-match))

(defun assert-not-posix-match (lineno regexp str &optional message)
  (elunit-differ regexp str (-elunit-lineno-message lineno message)
                 :test 'posix-string-match))

(defun assert-posix-match-equal (lineno regexp str expected &optional message)
  (assert-posix-match lineno regexp str message)
  (assert-string= lineno expected
                  (substring str (match-beginning 0) (match-end 0)) message))

(defun assert-posix-match-whole (lineno regexp str &optional message)
  (assert-posix-match lineno regexp str message)
  (assert-string= lineno str (substring str (match-beginning 0) (match-end 0))
                  message))

(provide 'elunit)
