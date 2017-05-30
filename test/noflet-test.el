;;; -*- lexical-binding: t -*-

(require 'noflet)
(require 'ert)
(require 'cl-lib)

(ert-deftest noflet-test-simple ()
  :tags '(ido ido-ubiquitous)
  "Test that basic noflet functionality is working."
  (should (= 3
             (noflet ((1+ (x) (+ 1 x)))
               (1+ 2)))))

(defun noflet-run-all-tests ()
  (interactive)
  (ert "^noflet-"))

(provide 'noflet-test)

;;; noflet-test.el ends here
