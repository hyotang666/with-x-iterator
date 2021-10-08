; vim: ft=lisp et
(in-package :asdf)
(defsystem "with-x-iterator.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "with-x-iterator")
  :components
  ((:file "with-x-iterator"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :with-x-iterator args)))