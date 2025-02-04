(defpackage integrators.integrator
  (:use :cl)
  (:export #:step
	   #:forward-euler
	   #:rk4
	   #:heun
	   #:solve)
  (:local-nicknames (:pl :petalisp)
		    (:ode :integrators.odeproblem)))

(in-package :integrators.integrator)


(defclass forward-euler ()
  ())

(defclass rk4 ()
  ())

(defclass heun ()
  ())

(defgeneric ode-step (integ problem y_n t_n)
  (:documentation "Generic function for integrator step."))

(defmethod ode-step ((integ forward-euler) problem y_n t_n)
  "Forward Euler numerical integration."
  (pl:compute (pl:lazy #'+ (pl:lazy-array y_n) (pl:lazy #'* (pl:lazy-array (funcall (slot-value problem 'ode:functional-form) y_n t_n)) (slot-value problem 'ode:t-step)))))

(defmethod ode-step ((integ heun) problem y_n t_n)
  "Heun numerical integration scheme."
  (let ((t-step (slot-value problem 'ode:t-step))
	(y_n (pl:lazy-array y_n))
	(k1 0)
	(k2 0)
	(k3 0))
    (setq k1 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form) y_n t_n)))
    (setq k2 (pl:lazy-array (pl:lazy #'+ y_n (pl:lazy #'* k1 t-step))))
    (setq k3 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form) k1 (+ t_n t-step))))
    (pl:compute (pl:lazy #'+ y_n (pl:lazy #'* (/ t-step 2)
					  (pl:lazy #'+ k1 k3))))))

(defmethod ode-step ((integ rk4) problem y_n t_n)
  "ODE-step for a Runge-Kutta integrator to 4th order."
  (let ((t-step (slot-value problem 'ode:t-step))
	(y_n (pl:lazy-array y_n))
	(k1 0)
	(k2 0)
	(k3 0)
	(k4 0))
    (setq k1 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form) y_n t_n)))
    (setq k2 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form)
				     (pl:lazy #'+ y_n (pl:lazy #'* k1 (/ t-step 2))) (+ t_n (/ t-step 2)))))
    (setq k3 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form)
				     (pl:lazy #'+ y_n (pl:lazy #'* k2 (/ t-step 2))) (+ t_n (/ t-step 2)))))
    (setq k4 (pl:lazy-array (funcall (slot-value problem 'ode:functional-form)
				     (pl:lazy #'+ y_n (pl:lazy #'* k3 t-step)) (+ t_n t-step))))
    (pl:compute (pl:lazy #'+ y_n
			 (pl:lazy #'* (/ t-step 6)
				  (pl:lazy #'+ k1 (pl:lazy #'* k2 2) (pl:lazy #'* k3 2) k4))))))

(defun solve (problem solver)
  (let ((times (loop for n from (slot-value problem 'ode:t-step) below (slot-value problem 'ode:t-max) by  (slot-value problem 'ode:t-step) collect n))
	(y_n (slot-value problem 'ode:init-cond))
	(sol (list (slot-value problem 'ode:init-cond))))
    (dolist (t_n times sol)
      (setf y_n (ode-step solver problem y_n t_n))
      (setq sol (append sol (list y_n))))
    sol))
