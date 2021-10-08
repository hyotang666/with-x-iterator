(in-package :cl-user)

(defpackage :with-x-iterator
  (:use :cl)
  (:export #:with-list-iterator
           #:with-vector-iterator
           #:with-plist-iterator
           #:with-alist-iterator
           #:with-sequence-iterator)
  (:export #:with-hash-table-iterator*
           #:with-list-iterator*
           #:with-vector-iterator*
           #:with-plist-iterator*
           #:with-alist-iterator*
           #:with-sequence-iterator*))

(in-package :with-x-iterator)

;;;;
;;;; Protocols.
;;;;
;;;; * Macro name must be WITH-<name>-ITERATOR.
;;;; * Macro API must be ((name <object>) &body body)
;;;; * Name must become generator-macro. (e.g. `(macrolet ((,name () ...))))
;;;; * Name macro must return three values as (values more? key object)
;;;;
;;;; In short words, each macros are follow CL:WITH-HASH-TABLE-ITERATOR behavior.

(defmacro with-list-iterator ((name <list>) &body body)
  (let ((?temp (gensym "TEMP")) (?index (gensym "INDEX")))
    `(let ((,?temp ,<list>) (,?index 0))
       (macrolet ((,name ()
                    (let ((?car (gensym "CAR")))
                      `(unless (endp ,',?temp)
                         (let ((,?car (car ,',?temp)))
                           (values (prog1 t (setq ,',?temp (cdr ,',?temp)))
                                   (prog1 ,',?index (incf ,',?index))
                                   ,?car))))))
         ,@body))))

(defmacro with-vector-iterator ((name <vector>) &body body)
  (let ((?vector (gensym "VECTOR")) (?index (gensym "INDEX")))
    `(let ((,?vector ,<vector>) (,?index 0))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(when (array-in-bounds-p ,',?vector ,',?index)
                         (let ((,?elt (aref ,',?vector ,',?index)))
                           (values t
                                   (prog1 ,',?index (incf ,',?index))
                                   ,?elt))))))
         ,@body))))

(defmacro with-plist-iterator ((name <plist>) &body body)
  (let ((?plist (gensym "PLIST")))
    `(let ((,?plist ,<plist>))
       (macrolet ((,name ()
                    (let ((?key (gensym "KEY")) (?value (gensym "VALUE")))
                      `(unless (endp ,',?plist)
                         (let ((,?key (car ,',?plist))
                               (,?value (cadr ,',?plist)))
                           (setq ,',?plist (cddr ,',?plist))
                           (values t ,?key ,?value))))))
         ,@body))))

(defmacro with-alist-iterator ((name <alist>) &body body)
  (let ((?alist (gensym "ALIST")))
    `(let ((,?alist ,<alist>))
       (macrolet ((,name ()
                    (let ((?pair (gensym "PAIR")))
                      `(unless (endp ,',?alist)
                         (let ((,?pair (car ,',?alist)))
                           (setq ,',?alist (cdr ,',?alist))
                           (values t (car ,?pair) (cdr ,?pair)))))))
         ,@body))))

(defmacro with-sequence-iterator ((name <sequence>) &body body)
  (let ((?sequence (gensym "SEQUENCE"))
        (?index (gensym "INDEX"))
        (?length (gensym "LENGTH")))
    `(let* ((,?sequence ,<sequence>) (,?index 0) (,?length (length ,?sequence)))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(when (< ,',?index ,',?length)
                         (let ((,?elt (elt ,',?sequence ,',?index)))
                           (values t
                                   (prog1 ,',?index (incf ,',?index))
                                   ,?elt))))))
         ,@body))))

;;;;
;;;; Protocols.
;;;;
;;;; * Macro name must be WITH-<name>-ITERATOR*.
;;;; * Macro API must be ((name <object>) &body body)
;;;; * Name must become generator-macro. (e.g. `(macrolet ((,name () ...))))
;;;; * Name macro must return three values as (values next? key object)
;;;;
;;;; Not like cl:with-hash-table-iterator, first value is become t if entry remains.

(defmacro with-hash-table-iterator* ((name <hash-table>) &body body)
  (let ((?hash-table (gensym "HASH-TABLE"))
        (?count (gensym "COUNT"))
        (?index (gensym "INDEX"))
        (?iter (gensym "ITER")))
    `(let* ((,?hash-table ,<hash-table>)
            (,?count (hash-table-count ,?hash-table))
            (,?index 0))
       (with-hash-table-iterator (,?iter ,<hash-table>)
         (macrolet ((,name ()
                      (let ((?more? (gensym "MORE?"))
                            (?key (gensym "KEY"))
                            (?value (gensym "VALUE")))
                        `(multiple-value-bind (,?more? ,?key ,?value)
                             (,',?iter)
                           (declare (ignore ,?more?))
                           (values (< (incf ,',?index) ,',?count)
                                   ,?key
                                   ,?value)))))
           ,@body)))))

(defmacro with-list-iterator* ((name <list>) &body body)
  (let ((?temp (gensym "TEMP")) (?index (gensym "INDEX")))
    `(let ((,?temp ,<list>) (,?index 0))
       (macrolet ((,name ()
                    (let ((?car (gensym "CAR")))
                      `(let ((,?car (car ,',?temp)))
                         (values (not (endp (setq ,',?temp (cdr ,',?temp))))
                                 (prog1 ,',?index (incf ,',?index))
                                 ,?car)))))
         ,@body))))

(defmacro with-vector-iterator* ((name <vector>) &body body)
  (let ((?vector (gensym "VECTOR")) (?index (gensym "INDEX")))
    `(let ((,?vector ,<vector>) (,?index 0))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(let ((,?elt (aref ,',?vector ,',?index)))
                         (values (array-in-bounds-p ,',?vector (1+ ,',?index))
                                 (prog1 ,',?index (incf ,',?index))
                                 ,?elt)))))
         ,@body))))

(defmacro with-plist-iterator* ((name <plist>) &body body)
  (let ((?plist (gensym "PLIST")))
    `(let ((,?plist ,<plist>))
       (macrolet ((,name ()
                    (let ((?key (gensym "KEY")) (?value (gensym "VALUE")))
                      `(let ((,?key (car ,',?plist)) (,?value (cadr ,',?plist)))
                         (values (not (endp (setq ,',?plist (cddr ,',?plist))))
                                 ,?key
                                 ,?value)))))
         ,@body))))

(defmacro with-alist-iterator* ((name <alist>) &body body)
  (let ((?alist (gensym "ALIST")))
    `(let ((,?alist ,<alist>))
       (macrolet ((,name ()
                    (let ((?pair (gensym "PAIR")))
                      `(let ((,?pair (car ,',?alist)))
                         (values (not (endp (setq ,',?alist (cdr ,',?alist))))
                                 (car ,?pair)
                                 (cdr ,?pair))))))
         ,@body))))

(defmacro with-sequence-iterator* ((name <sequence>) &body body)
  (let ((?sequence (gensym "SEQUENCE"))
        (?index (gensym "INDEX"))
        (?length (gensym "LENGTH")))
    `(let* ((,?sequence ,<sequence>) (,?index 0) (,?length (length ,?sequence)))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(let ((,?elt (elt ,',?sequence ,',?index)))
                         (values (< (1+ ,',?index) ,',?length)
                                 (prog1 ,',?index (incf ,',?index))
                                 ,?elt)))))
         ,@body))))