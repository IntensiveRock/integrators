# Integrators - Numerical Integrators in Common Lisp

## Usage
This library solves ordinary differential equations for scalar and vector-valued functions of the form $\frac{dy}{dt}=f(y,t)$. As of this commit, there are 3 solvers included:
1. Forward Euler (works but not recommended for most use cases)
2. Heun (RK2)
3. Runge-Kutta to fourth order (RK4)

There are two stages to solving your ODEs: defining the problem and solving the problem.
### Defining the problem.
In this project, we define a consistent problem structure agnostic to the form of the ODE you would like to solve. This is accomplished by defining a class, ode-problem, that stores all the problem-specific information necessary for the solver. This is best illustrated through the following tutorial/example. Consider the following differential equation:

$$\frac{dy}{dt}=3, \quad\quad y(0)=1.$$

If I would like to solve this ODE to a maximum time, $t_{max}=10$, and a time step, $\delta t=0.01$, I can define the problem for this ODE like this:

```common-lisp
(make-instance 'ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t_max 10 :t-step 0.01)
```

This object has 4 slots that the solver will access:
1. functional-form (:func)
2. init-cond (:y_0)
3. t-max (:t-max)
4. t-step (:t-step)

Now that we have defined our problem, we must select a solver and solve.
### Solving the defined problem.
We have implemented the solver as a generic function that solves the ODE in a way determined by the solver passed to it. Below is the recommended way to solve the ODE (as of now the result of solving is a list so I will convert the result to an array, but in principle, this is not necessary). Here, we will employ the Forward Euler solver as this differential should be exactly solvable.

```common-lisp
(let ((problem (make-instance 'ode-problem :func #'(lambda (y_n t_n) 3) :y_0 1 :t-max 10 :t-step 0.01))
      (fe (make-instance 'forward-euler)))
    (make-array '(1000) :initial-contents (solve problem fe)))
```
Here, the result is an array of values containing the solutions to your ODE as a function of time.
### Advanced example (Modeling chemical kinetics).
Suppose I would like to study the following elementary reaction:

$$A\leftrightharpoons B$$

Here, the forward reaction has the rate constant, $k_1$, and the backward reaction has a rate $k_{-1}$. I can then write two coupled differential equations in terms of dimensionless time, $\tau=tk_1$, and a new rate constant, $K=k_1/k_{-1}$:

$$\frac{dA}{d\tau}=-A+\frac{1}{K}B$$

$$\frac{dB}{d\tau}=A-\frac{1}{K}B$$

I can rephrase this problem in terms of a vector of concentrations, $\vec{C}$ and a matrix as the vector-valued function, $M(\vec{C},\tau)$:

$$\frac{d\vec{C}}{d\tau}=M(y,\tau)=M\vec{C}$$

Suppose we have the initial condition, $A(0)=1,B(0)=0)$; we set up this problem in the following code block:

```common-lisp
(defparameter *k* 0.1)
;; Use your favorite matrix multiplication function! This is a placeholder.
(defun *M* (c-vec tau)
  (matmul #2a((-1 (\ 1 *k*)) (1 (\ -1 *k*)))))
;; This will solve the problem, it is recommended to save the result if you would like to use it later!
(let ((problem (make-instance 'ode-problem :func #'*M* :y_0 #(1 0) :t-max 10 :t-step 0.01))
      (fe (make-instance 'heun)))
    (make-array '(1000 2) :initial-contents (solve problem fe)))
```

## Installation
Installation is now available through Ultralisp! Make sure that you have configured quicklisp to install via Ultralisp and you can ql:quickload or qlot add the package!
