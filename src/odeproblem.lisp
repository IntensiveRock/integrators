;;; Define the ODEProblem.
(in-package :integrators)

(defclass ode-problem ()
  ((functional-form
    :initarg :func)
   (init-cond
    :initarg :y_0)
   (t-max
    :initarg :t-max)
   (t-step
    :initarg :t-step)))


