(defpackage integrators/tests/solvetest
  (:use :cl
	:rove)
  (:local-nicknames (:pl :petalisp)
		    (:ode :integrators.odeproblem)
		    (:integ :integrators.integrator))
  )

(in-package :integrators/tests/solvetest)

(defun **2 (x)
  (* x x))

(defun mse-svf (arr0 arr1)
  "Calculate the MSE between two arrays."
  (let ((N (array-total-size arr0))
	(diff-sq (pl:compute (pl:lazy-array (pl:lazy-reduce #'+ (pl:lazy #'**2 (pl:lazy #'- arr0 arr1)))))))
    (/ diff-sq N)))

(defun mse-vvf (arr0 arr1)
  "Calculate the MSE between two arrays."
  (let ((avg 0.)
	(N (array-total-size arr0))
	(diff-sq (pl:compute (pl:lazy-array (pl:lazy-reduce #'+ (pl:lazy-reduce #'+ (pl:lazy #'**2 (pl:lazy #'- arr0 arr1))))))))
    (/ diff-sq N)))

;;; Scalar tests

(defun fe-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:forward-euler))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
    (> 0.00000001 (mse-svf true-solution solution))))

(defun rk4-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:rk4))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse-svf true-solution solution))))

(defun heun-test ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:heun))
	(true-solution (make-array '(100) :initial-contents (loop for n from 0 below 10 by 0.1 collect (+ 1. (* 3. n))))))
    (setf solution (make-array '(100) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse-svf true-solution solution))))

;;; Vector tests.

(defun fe-test-v ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 #(1 1) :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:forward-euler))
	(true-solution (make-array '(100 2) :initial-contents (loop for n from 0 below 10 by 0.1 collect (list (+ 1. (* 3. n)) (+ 1. (* 3. n)))))))
    (setf solution (make-array '(100 2) :initial-contents (integ:solve problem fe)))
    (> 0.00000001 (mse-vvf true-solution solution))))

(defun rk4-test-v ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 #(1 1) :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:rk4))
	(true-solution (make-array '(100 2) :initial-contents (loop for n from 0 below 10 by 0.1 collect (list (+ 1. (* 3. n)) (+ 1. (* 3. n)))))))
    (setf solution (make-array '(100 2) :initial-contents (integ:solve problem fe)))
    (> 0.00000001 (mse-vvf true-solution solution))))

(defun heun-test-v ()
  (let ((problem (make-instance 'ode:ode-problem :func #'(lambda (y_n t_n) 3) :y_0 #(1 1) :t-max 10 :t-step 0.1))
	(fe (make-instance 'integ:heun))
	(true-solution (make-array '(100 2) :initial-contents (loop for n from 0 below 10 by 0.1 collect (list (+ 1. (* 3. n)) (+ 1. (* 3. n)))))))
    (setf solution (make-array '(100 2) :initial-contents (integ:solve problem fe)))
   (> 0.00000001 (mse-vvf true-solution solution))))

(deftest Forward-Euler
  (testing "Forward Euler S"
    (ok (fe-test)))
  (testing "Forward Euler V"
    (ok (fe-test-v))))
(deftest Heun
  (testing "Heun S"
    (ok (heun-test)))
  (testing "Heun V"
    (ok (heun-test-v))))
(deftest RK4
  (testing "RK4 S"
    (ok (rk4-test)))
  (testing "RK4 V"
    (ok (rk4-test-v))))

(run-suite *package*)
