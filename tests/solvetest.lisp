(defpackage integrators/tests/solvetest
  (:use :cl
	:rove)
  ;; (:export )
  (:local-nicknames (:pl :petalisp)
		    (:ode :integrators.odeproblem)
		    (:integ :integrators.integrator))
  )

(in-package :integrators/tests/solvetest)

(defun **2 (x)
  (* x x))

(defun mse (arr0 arr1)
  "Calculate the MSE between two arrays."
  (let ((avg 0.)
	(N (length arr0))
	(diff-sq (pl:compute (pl:lazy #'**2 (pl:lazy #'- arr0 arr1)))))
    (dotimes (i N)
	  (setf avg (+ avg (aref diff-sq i))))
    (/ avg N)))

(defun fe-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:forward-euler))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse true-solution solution))
    ))

(defun rk4-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:rk4))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse true-solution solution))
    ))

(defun heun-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:heun))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse true-solution solution))
    ))

(deftest integrator-test
  (testing "Forward Euler Solver"
    (ok (fe-test)))
  (testing "RK4 Test"
    (ok (rk4-test)))
  (testing "Heun Test"
    (ok (heun-test))))
(run-suite *package*)
