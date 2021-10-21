(in-package :cl-user)

(defpackage :with-x-iterator
  (:use :cl)
  (:export ;;;; Main API
           ;;; Iterator.
           #:do+
           ;; Generator.
           #:generator)
  (:export ;;;; Underlying generators.
           #:list-generator
           #:vector-generator
           #:sequence-generator
           #:hash-table-generator
           #:integer-generator)
  (:export ;;;; Underlying iterators. API is inherit from cl:with-hash-table-iterator.
           #:with-list-iterator
           #:with-vector-iterator
           #:with-sequence-iterator
           #:with-integer-iterator
           #:with-generator-iterator
           #:with-x-iterator))

(in-package :with-x-iterator)

;;;; Underlying iterators, API is inherited from CL:WITH-HASH-TABLE-ITERATOR.

(defmacro with-list-iterator ((name <list>) &body body)
  (let ((?temp (gensym "TEMP")) (?index (gensym "INDEX")))
    `(let ((,?temp ,<list>) (,?index 0))
       (declare (type (mod #.most-positive-fixnum) ,?index))
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
       (declare (type (mod #.array-total-size-limit) ,?index))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(when (array-in-bounds-p ,',?vector ,',?index)
                         (let ((,?elt (aref ,',?vector ,',?index)))
                           (values t
                                   (prog1 ,',?index (incf ,',?index))
                                   ,?elt))))))
         ,@body))))

(defmacro with-sequence-iterator ((name <sequence>) &body body)
  (let ((?sequence (gensym "SEQUENCE"))
        (?index (gensym "INDEX"))
        (?length (gensym "LENGTH")))
    `(let* ((,?sequence ,<sequence>) (,?index 0) (,?length (length ,?sequence)))
       (declare (type (mod #.most-positive-fixnum) ,?index))
       (macrolet ((,name ()
                    (let ((?elt (gensym "ELT")))
                      `(when (< ,',?index ,',?length)
                         (let ((,?elt (elt ,',?sequence ,',?index)))
                           (values t
                                   (prog1 ,',?index (incf ,',?index))
                                   ,?elt))))))
         ,@body))))

(defmacro with-integer-iterator
          ((name limit &key (by 1) (limitter '#'<) (start 0)) &body body)
  (let ((?index (gensym "INDEX"))
        (?integer (gensym "INTEGER"))
        (?by (gensym "BY"))
        (?limit (gensym "LIMIT")))
    (if limit
        `(let ((,?index 0) (,?integer ,start) (,?by ,by) (,?limit ,limit))
           (macrolet ((,name ()
                        `(if (funcall ,',limitter ,',?integer ,',?limit)
                             (values t
                                     (prog1 ,',?index (incf ,',?index))
                                     (prog1 ,',?integer
                                       (setq ,',?integer
                                               (+ ,',?integer ,',?by)))))))
             ,@body))
        `(let ((,?index 0) (,?integer ,start) (,?by ,by))
           (macrolet ((,name ()
                        `(values t
                                 (prog1 ,',?index (incf ,',?index))
                                 (prog1 ,',?integer
                                   (setq ,',?integer (+ ,',?integer ,',?by))))))
             ,@body)))))

(defmacro with-generator-iterator ((name <generator>) &body body)
  (let ((fun-name
         (typecase <generator>
           ((or (cons (eql quote) (cons symbol null))
                (cons (eql function)
                      (cons (or symbol (cons (eql lambda))) null)))
            (cadr <generator>))
           ((cons (eql lambda)) <generator>))))
    (if fun-name
        `(macrolet ((,name ()
                      `(,',fun-name)))
           ,@body)
        (let ((?generator (gensym "GENERATOR")))
          `(let ((,?generator ,<generator>))
             (macrolet ((,name ()
                          `(funcall ,',?generator)))
               ,@body))))))

(defmacro with-x-iterator ((name <x>) &body body)
  (let ((?generator (gensym "GENERATOR")))
    `(let ((,?generator (generator ,<x>)))
       (macrolet ((,name ()
                    `(funcall ,',?generator)))
         ,@body))))

;;;; GENERATORS.

(declaim
 (ftype (function * (values function &optional))
        list-generator
        vector-generator
        sequence-generator
        hash-table-generator
        integer-generator))

(defun list-generator (list)
  (with-list-iterator (get? list)
    (flet ((generator ()
             (get?)))
      #'generator)))

(defun vector-generator (vector)
  (with-vector-iterator (get? vector)
    (flet ((generator ()
             (get?)))
      #'generator)))

(defun sequence-generator (sequence)
  (with-sequence-iterator (get? sequence)
    (flet ((generator ()
             (get?)))
      #'generator)))

(defun hash-table-generator (hash-table)
  (with-hash-table-iterator (get? hash-table)
    (flet ((generator ()
             (get?)))
      #'generator)))

(defun integer-generator (&key (start 0) limit (by 1) (limitter #'<))
  (with-integer-iterator (get? limit :start start :by by :limitter limitter)
    (flet ((generator ()
             (get?)))
      #'generator)))

(defgeneric generator (thing)
  (:documentation "Return (FUNCTION () (VALUES Existsp Key Value)).")
  (:method ((thing list)) (list-generator thing))
  (:method ((thing vector)) (vector-generator thing))
  (:method ((thing sequence)) (sequence-generator thing))
  (:method ((thing hash-table)) (hash-table-generator thing))
  (:method ((thing integer)) (integer-generator :limit thing))
  (:method ((thing function)) thing))

(define-compiler-macro generator (&whole whole thing &environment env)
  (let ((type (form-type thing env)))
    (cond ((subtypep type 'vector) `(vector-generator ,thing))
          ((subtypep type 'list) `(list-generator ,thing))
          ((subtypep type 'sequence) `(sequence-generator ,thing))
          ((subtypep type 'hash-table) `(hash-table-generator ,thing))
          ((subtypep type 'integer) `(integer-generator :limit ,thing))
          ((subtypep type 'function) thing)
          (t whole))))

;;;; DO+

(defun rebuild-declare (bind env)
  (let ((vars
         (intersection bind
                       (tcr.parse-declarations-1.0:declaration-env.affected-variables
                         env))))
    (when vars
      (tcr.parse-declarations-1.0:build-declarations 'declare
                                                     (tcr.parse-declarations-1.0:filter-declaration-env
                                                       env
                                                       :affecting vars)))))

(defun <iteration> (bind* end body ?generators)
  "Responds to achieve iteration."
  (multiple-value-bind (forms decls)
      (alexandria:parse-body body)
    (let ((?top (gensym "ITER"))
          (decl-env (tcr.parse-declarations-1.0:parse-declarations decls))
          (?ends (alexandria:make-gensym-list (length bind*))))
      `((block nil
          (tagbody
           ,?top
           ,@(labels ((<iteration> (binds vars generators)
                        (if (endp binds)
                            `((tagbody
                                (when (or ,@(mapcar
                                              (lambda (?end) `(null ,?end))
                                              ?ends)
                                          ,(car end))
                                  (return ,@(cdr end)))
                               ,@forms
                                (go ,?top)))
                            (let ((bind (bind (car binds))))
                              `((multiple-value-bind (,(car vars) ,@bind)
                                    (,(car generators))
                                  ,@(unless (eq bind (caar binds))
                                      `((declare (ignore ,(car bind)))))
                                  ,@(rebuild-declare bind decl-env)
                                  ,@(<iteration> (cdr binds) (cdr vars)
                                                 (cdr generators)))))))
                      (bind (bind)
                        (etypecase (car bind)
                          (symbol (list (gensym "KEY") (car bind)))
                          (cons (car bind)))))
               (<iteration> bind* ?ends ?generators))))))))

(defun form-type (form env)
  "Return canonicalized form type."
  (let ((type (cl-form-types:form-type form env)))
    (typecase type
      ((cons (eql values)) (setq type (cadr type)))
      ((cons (eql function)) (setq type 'function)))
    type))

(defun <do+> (bind* end body env)
  "Responds to achieve environment of generators."
  (labels ((rec (binds &optional generators)
             (if (endp binds)
                 (<iteration> bind* end body (nreverse generators))
                 (let ((type (form-type (cadar binds) env))
                       (?generate (gensym "GENERATE")))
                   `((,(<with-x-iterator> type (cadar binds))
                      (,?generate ,(cadar binds))
                      ,@(rec (cdr binds) (cons ?generate generators)))))))
           (<with-x-iterator> (type form)
             (cond
               ((or (subtypep type 'list)
                    ;; KLUDGE: poor code walk.
                    (typep form '(cons (eql list-generator))))
                'with-list-iterator)
               ((or (subtypep type 'vector)
                    (typep form '(cons (eql vector-generator))))
                'with-vector-iterator)
               ((or (subtypep type 'sequence)
                    (typep form '(cons (eql sequence-generator))))
                'with-sequence-iterator)
               ((or (subtypep type 'hash-table)
                    (typep form '(cons (eql hash-table-generator))))
                'with-hash-table-iterator)
               ((or (subtypep type 'integer)
                    (typep form '(cons (eql integer-generator))))
                'with-integer-iterator)
               ((subtypep type 'function) 'with-generator-iterator)
               (t 'with-x-iterator))))
    (rec bind*)))

(defmacro do+ ((&rest bind*) (&rest end) &body body &environment env)
  "(DO+ ({([ Value-var | (Key-var Value-var) ] Generator)}+) (Test Exit-form*) Declaration* Form*)"
  ;; Trivial-syntax-checking.
  (assert (< 0 (length bind*)))
  (loop :for bind :in bind*
        :do (check-type (car bind)
                        (or symbol
                            (cons (and symbol (not (or keyword boolean)))
                                  (cons (and symbol (not (or keyword boolean)))
                                        null)))))
  (car (<do+> bind* end body env)))