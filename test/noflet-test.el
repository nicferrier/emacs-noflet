;;; -*- lexical-binding: t -*-

(require 'noflet)
(require 'ert)
(require 'cl-lib)

;; This is a series of macros to facilitate the non-interactive
;; testing of interactive functions by simulating user input.

(defmacro keyboard-quit-to-error (&rest body)
  "Evaluate BODY but signal an error on `keyboard-quit'."
  `(condition-case nil
       (progn ,@body)
     (quit
      (error "Caught `keyboard-quit'"))))

(defmacro with-simulated-input (keys &rest body)
  "Eval body with KEYS as simulated input.

This macro is intended for testing normally interactive functions
by simulating input. If BODY tries to read more input events than
KEYS provides, `keyboard-quit' is invoked (by means of appending
multple C-g keys to KEYS). This is to ensure that BODY will never
block waiting for input, since this macro is intended for
noninteractive use. As such, BODY should not invoke
`keyboard-quit' under normal operation, and KEYS should not
include C-g, or this macro will interpret it as reading past the
end of input."
  ;; It would be better to detect end-of-input by overriding
  ;; `read-event' to throw an error, since theoretically C-g could be
  ;; rebound to something other than `keyboard-quit'. But apparently
  ;; some functions read input directly in C code, and redefining
  ;; `read-event' has no effect on those. So the suboptimal solution
  ;; is to rely on C-g.
  (declare (indent 1))
  `(let* ((key-sequence (listify-key-sequence (kbd ,keys)))
          (C-g-key-sequence
           (listify-key-sequence
            ;; We *really* want to trigger `keyboard-quit' if we reach
            ;; the end of the input.
            (kbd "C-g C-g C-g C-g C-g C-g C-g")))
          (unread-command-events
           (append key-sequence C-g-key-sequence)))
     (when (member (car C-g-key-sequence) key-sequence)
       (error "KEYS must not include C-g"))
     (condition-case nil
         (progn ,@body)
       (quit
        (error "Reached end of simulated input while evaluating body")))))

(ert-deftest noflet-test-simple ()
  :tags '(ido ido-ubiquitous)
  "Test that basic noflet functionality is working."
  (noflet ((1+ (x) (+ 1 x)))
    (should (= 3 (1+ 2)))))

(ert-deftest noflet-test-this-fn ()
  "Test that `this-fn' can be used in noflet function bodies."
  (noflet ((1+ (x) (funcall this-fn x)))
    (should (= 3 (1+ 2)))))

(ert-deftest noflet-test-letbind ()
  "Test that let-bound variables are accessible inside function bodies."
  (let ((myvar 5))
    (noflet ((myfun (x) (1+ x)))
      (myfun myvar))))

(ert-deftest noflet-test-interactive-command ()
  "Test that interactive forms can be used in noflet function bodies."
  (noflet ((myfun (x) (interactive "p") (1+ x)))
    (should (= 5 (myfun 4)))
    (should (= 3 (let ((current-prefix-arg '(2))) (call-interactively 'myfun))))))

(ert-deftest noflet-test-overriding-command ()
  "Test that overriding a command preserves its interactive form.

Also test that if the overriding function has its own interactive
form, that takes precedence."
  (let ((libpath (locate-library "simple")))
    (should libpath)
    (noflet ((locate-library
            (&rest args)
            (apply this-fn args)))
      ;; Try calling non-interactively
      (should (string= libpath (locate-library "simple")))
      ;; Try calling interactively
      (should
       (string=
        libpath
        (with-simulated-input "simple RET"
          (call-interactively 'locate-library)))))))

(ert-deftest noflet-test-alias ()
  "Test that noflet can alias one function to another."
  ;; Failure happens during macroexpansion, so `eval' is needed to
  ;; defer expansion until the test is running.
  (eval
   '(noflet ((my-1+ 1+))
      (should (= 3 (my-1+ 2))))))

(ert-deftest noflet-test-swap-function-definitions ()
  "Test that noflet can swap two function definitions."
  (cl-letf (((symbol-function 'myfun1) (lambda () 1))
            ((symbol-function 'myfun2) (lambda () 2)))
    ;; Verify that functions work as expected first
    (should (= (myfun1) 1))
    (should (= (myfun2) 2))
    ;; Now swap them
    (eval
     '(noflet ((myfun1 myfun2)
               (myfun2 myfun1))
        (should (= (myfun1) 2))
        (should (= (myfun2) 1))))))

(ert-deftest noflet-test-cl-argspec ()
  "Test that CL-style arguments can be used in noflet bindings."
  (noflet ((my-cl-fun
            (arg &key key1 key2)
            (list arg key1 key2)))
    (should
     (equal '(a b c)
            (my-cl-fun 'a :key1 'b :key2 'c)))
    (should-error (my-cl-fun))
    (should-error (my-cl-fun 'a :badkey 'b))))

(defun noflet-run-all-tests ()
  (interactive)
  (ert "^noflet-"))

(provide 'noflet-test)

;;; noflet-test.el ends here
