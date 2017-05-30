;;; -*- lexical-binding: t -*-

(require 'noflet)
(require 'ert)
(require 'cl-lib)

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

(defun noflet-run-all-tests ()
  (interactive)
  (ert "^noflet-"))

(provide 'noflet-test)

;;; noflet-test.el ends here
