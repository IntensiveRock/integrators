(defpackage integrators.odeproblem
  (:use :cl)
  (:export #:ode-problem
	   #:functional-form
	   #:t-max
	   #:t-step
	   #:init-cond)
  (:local-nicknames (:pl :petalisp)))

(in-package :integrators.odeproblem)

(defclass ode-problem ()
  ((functional-form
    :initarg :func)
   (init-cond
    :initarg :y_0)
   (t-max
    :initarg :t-max)
   (t-step
    :initarg :t-step)))


