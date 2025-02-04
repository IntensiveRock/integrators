(defpackage integrators.integrator
  (:use :cl)
  (:export #:step
	   #:forward-euler
	   #:solve)
  (:local-nicknames (:pl :petalisp)
		    (:ode :integrators.odeproblem)))

(in-package :integrators.integrator)


(defclass forward-euler ()
  ())

(defgeneric ode-step (integ problem y_n t_n)
  (:documentation "Generic function for integrator step."))

(defmethod ode-step ((integ forward-euler) problem y_n t_n)
  (+ y_n (* (slot-value problem 'ode:t-step) (funcall (slot-value problem 'ode:functional-form) y_n t_n)
	  )))

(defun solve (problem solver)
  (let ((times (loop for n from (slot-value problem 'ode:t-step) below (slot-value problem 'ode:t-max) by  (slot-value problem 'ode:t-step) collect n))
	(y_n (slot-value problem 'ode:init-cond))
	(sol (list (slot-value problem 'ode:init-cond))))
    (dolist (t_n times sol)
      (setf y_n (ode-step solver problem y_n t_n))
      (setq sol (append sol (list y_n))))
    sol))
