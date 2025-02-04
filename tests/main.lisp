(defpackage integrators/tests/main
  (:use :cl
        :integrators
        :rove))
(in-package :integrators/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :integrators)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
