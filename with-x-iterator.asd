; vim: ft=lisp et
(in-package :asdf)
(defsystem "with-x-iterator"
  :version
  "2.0.1"
  :author "SATO Shinichi"
  :description "CL:WITH-HASH-TABLE-ITERATOR like macros for other data structures."
  :license "Public domain"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "with-x-iterator")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "with-x-iterator").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "with-x-iterator"))))
  (append (call-next-method) '((test-op "with-x-iterator.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "with-x-iterator")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
