(defsystem "integrators"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("trivial-features"
	       "petalisp")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "odeproblem")
		 (:file "integrator"))))
  :description "Integrators in Common Lisp"
  :in-order-to ((test-op (test-op "integrators/tests"))))

(defsystem "integrators/tests"
  :author ""
  :license ""
  :depends-on ("integrators"
	       "petalisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "solvetest"))))
  :description "Test system for integrators"
  :perform (test-op (op c) (symbol-call :rove :run c)))
