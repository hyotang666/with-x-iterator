(in-package :cl-user)

(defpackage :with-x-iterator
  (:use :cl)
  (:export ;;;; Main API
           ;;; Iterator.
           #:do+
           ;; Generator.
           #:generator)
  (:export ;;;; Underlying helper.
           #:list-generator
           #:vector-generator
           #:sequence-generator
           #:hash-table-generator))

(in-package :with-x-iterator)

(defmacro do+ ((&rest bind*) (&rest end) &body body)
  "(DO+ ({([ Value-var | (Key-var Value-var) ] Generator)}+) (Test Exit-form*) Declaration* Form*)"
  ;; Trivial-syntax-checking.
  (assert (< 0 (length bind*)))
  (loop :for bind :in bind*
        :do (check-type (car bind)
                        (or symbol
                            (cons (and symbol (not (or keyword boolean)))
                                  (cons (and symbol (not (or keyword boolean)))
                                        null)))))
  (let ((?ends (alexandria:make-gensym-list (length bind*)))
        (?top (gensym "ITER"))
        (?generators (alexandria:make-gensym-list (length bind*))))
    (multiple-value-bind (forms decls)
        (alexandria:parse-body body)
      `(prog ,(mapcar (lambda (?generator bind) `(,?generator ,(cadr bind)))
                      ?generators bind*)
        ,?top
        ,@(labels ((<iteration> (binds vars generators)
                     (if (endp binds)
                         `((tagbody
                             (when (or ,@(mapcar (lambda (?end) `(null ,?end))
                                                 ?ends)
                                       ,(car end))
                               (return ,@(cdr end)))
                             (locally ,@decls ,@forms)
                             (go ,?top)))
                         (let ((bind (bind (car binds))))
                           `((multiple-value-bind (,(car vars) ,@bind)
                                 (funcall ,(car generators))
                               ,@(unless (eq bind (caar binds))
                                   `((declare (ignore ,(car bind)))))
                               ,@(<iteration> (cdr binds) (cdr vars)
                                              (cdr generators)))))))
                   (bind (bind)
                     (etypecase (car bind)
                       (symbol (list (gensym "KEY") (car bind)))
                       (cons (car bind)))))
            (<iteration> bind* ?ends ?generators))))))

(defun list-generator (list)
  (let ((index 0))
    (flet ((generate ()
             (if (endp list)
                 nil
                 (values t
                         (prog1 index (incf index))
                         (prog1 (car list) (setq list (cdr list)))))))
      #'generate)))

(defun vector-generator (vector)
  (let ((index 0))
    (flet ((generate ()
             (if (array-in-bounds-p vector index)
                 (values t index (aref vector (prog1 index (incf index))))
                 nil)))
      #'generate)))

(defun sequence-generator (sequence)
  (let ((length (length sequence)) (index 0))
    (flet ((generate ()
             (if (= index length)
                 nil
                 (values t index (elt sequence (prog1 index (incf index)))))))
      #'generate)))

(defun hash-table-generator (hash-table)
  (with-hash-table-iterator (get? hash-table)
    (flet ((generate ()
             (get?)))
      #'generate)))

(defun integer-generator (&key (start 0) limit (stepper #'1+) (limitter #'<))
  (if limit
      (let ((step start) (index 0))
        (lambda ()
          (if (funcall limitter step limit)
              (values t
                      (prog1 index (incf index))
                      (prog1 step (setq step (funcall stepper step))))
              nil)))
      (let ((step start) (index 0))
        (lambda ()
          (values t
                  (prog1 index (incf index))
                  (prog1 step (setq step (funcall stepper step))))))))

(defgeneric generator (thing)
  (:documentation "Return (FUNCTION () (VALUES Existsp Key Value)).")
  (:method ((thing list)) (list-generator thing))
  (:method ((thing vector)) (vector-generator thing))
  (:method ((thing sequence)) (sequence-generator thing))
  (:method ((thing hash-table)) (hash-table-generator thing))
  (:method ((thing integer)) (integer-generator :limit thing)))

(define-compiler-macro generator (&whole whole thing &environment env)
  (let ((type (cl-form-types:form-type thing env)))
    (cond ((subtypep type 'vector) `(vector-generator ,thing))
          ((subtypep type 'list) `(list-generator ,thing))
          ((subtypep type 'sequence) `(sequence-generator ,thing))
          ((subtypep type 'hash-table) `(hash-table-generator ,thing))
          ((subtypep type 'integer) `(integer-generator :limit ,thing))
          (t whole))))