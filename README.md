# WITH-X-ITERATOR 4.0.0
## What is this?
Trivial Iterator/Generator.

## Alternatives and differences.
| name                 | External-symbols | Extensible? | Generic? |
| -------------------- | ---------------- | ----------- | -------- |
| [series]             | 84               |             | No       |
| [iterate]            | 54               | Yes         | No       |
| [for]                | 85               | Yes         | Yes      |
| [cl-enumeration]     | 33               | Yes         | Yes      |
| [cl-tertools]        | 36               | Yes         | Yes      |
| [iterator-protocol]  | 5                | Yes         | Yes      |
| [doplus]             | 50               | Yes         | No       |
| [picl]               | 31               | Yes         | Yes      |
| [trivial-do]         | 6                | No          | No       |
| with-x-iterator      | 13               | Yes         | Yes      |

[series]: http://series.sourceforge.net/
[iterate]: https://gitlab.common-lisp.net/iterate/iterate
[for]: https://github.com/Shinmera/for
[cl-enumeration]: https://gitlab.common-lisp.net/cl-enumeration/enumerations
[cl-tertools]: https://github.com/mabragor/cl-itertools
[iterator-protocol]: https://github.com/jaeschliman/com.clearly-useful.iterator-protocol
[doplus]: https://github.com/alessiostalla/doplus
[picl]: https://github.com/anlsh/picl
[trivial-do]: https://github.com/yitzchak/trivial-do

## Usage

```lisp
(do+ ((var (generator '(1 2 3))))
  ()
  (print var))
```

Syntax: (DO+ ({([ Value-var | (Key-var Value-var) ] Generator)}+) (Test Exit-form\*) Declaration\* Form\*)

For minimizing the learning cost, with-x-iterator does not provide any useful clauses.

Use any features that you want to use.
E.g. for accumulation, you can use `uiop:while-collecting`.

```lisp
(uiop:while-collecting (collect)
  (do+ ((char "hoge"))
    ()
    (collect char)))
=> (#\h #\o #\g #\e)
```

For usability and efficiency, with-x-iterator infers the generator types.
If successfully infers the types from environments, no runtime funcall occurs.

```lisp
(macroexpand
  '(do+ ((i '(1 2 3)))
     ()
     (print i)))

(LET ((#:TEMP1809 '(1 2 3)) (#:INDEX1810 0))
  (DECLARE (TYPE (MOD 4611686018427387903) #:INDEX1810))
  (MACROLET ((#:GENERATE1805 () ; <--- inlined generator.
               (LET ((?CAR (GENSYM "CAR")))
                 `(UNLESS (ENDP ,'#:TEMP1809)
                    (LET ((,?CAR (CAR ,'#:TEMP1809)))
                      (VALUES (PROG1 T (SETQ ,'#:TEMP1809 (CDR ,'#:TEMP1809)))
                              (PROG1 ,'#:INDEX1810 (INCF ,'#:INDEX1810))
                              ,?CAR))))))
    (BLOCK NIL
      (TAGBODY
       #:ITER1806
        (MULTIPLE-VALUE-BIND (#:G1807 #:KEY1808 I)
            (#:GENERATE1805)
          (DECLARE (IGNORE #:KEY1808))
          (TAGBODY
            (WHEN (OR (NULL #:G1807) NIL) (RETURN))
            (PRINT I)
            (GO #:ITER1806)))))))
```

If failed to infer the types, fallback to runtime dispatch.

```lisp
(macroexpand
  '(do+ ((i int))
     ()
     (print i)))

(LET ((#:GENERATOR1969 (GENERATOR INT))) ; <--- Runtime dispatching.
  (MACROLET ((#:GENERATE1965 ()
               `(FUNCALL ,'#:GENERATOR1969))) ; <--- Inner loop funcall. Not efficient.
    (BLOCK NIL
      (TAGBODY
       #:ITER1966
        (MULTIPLE-VALUE-BIND (#:G1967 #:KEY1968 I)
            (#:GENERATE1965)
          (DECLARE (IGNORE #:KEY1968))
          (TAGBODY
            (WHEN (OR (NULL #:G1967) NIL) (RETURN))
            (PRINT I)
            (GO #:ITER1966)))))))
```

You can use helpers explicitly.

```lisp
(do+ ((elt (list-generator '(1 2 3))))
  ()
  (print elt))
```

For usability, generic-function `generator` is provided.

```lisp
(do+ ((elt (generator '(1 2 3))))
  ()
  (print elt))
```

For maxmizing the efficiency, `generator` has compiler macros.

```lisp
(funcall (compiler-macro-function 'generator) '(generator '(1 2 3)) nil)
=> (LIST-GENERATOR '(1 2 3))
```

With-x-iterator is designed as protocol-based.
Any generator function is acceptable.

For minimizing the learning cost, the generator functions protocols are designed as `(FUNCTION () (VALUES Existp Key Value))`.
You know this API is inherited from `cl:with-hash-table-iterator`.

To access the `Key` value, `do+` accepts two vars as its bindings.

```lisp
(uiop:while-collecting (ks vs)
  (do+ (((k v) (alexandria:plist-hash-table '(:a :b :c :d))))
    ()
    (ks k)
    (vs v)))
=> (:A :C)
   (:B :D)
```

Example of a generator to generate random value.
```lisp
(uiop:while-collecting (acc)
  (do+ (((times random) (let ((index 0))
			  (lambda ()
			    (values t (prog1 index (incf index))
				    (random 10))))))
    ((= 5 times))
    (acc random)))
```

## From developer
### The history.

1. I wrote the structure like `(defstruct struct (data nil :type list))`.
2. I wrote the code like `(dolist (elt (struct-data struct)) ...)`.
3. Ooops, I had to change slot data types.
4. I must change many places in my code base.
5. I reflected on it, so I wrote tiny abstraction barriar ``(defmacro dodata (o) `(dolist (elt (struct-data ,o)) ...))``.
6. Ooops, I got name confliction, e.g. `dodata` for `(defstruct obj (data nil :type list))`.
7. I realized what I want is generic looping.
8. [series], [iterate] and [doplus] is not good due to not generic.
9. [cl-enumeration] and [iterator-protocol] are not good due to heavy clos using.
10. [cl-itertools] and [picl] is built on top of [iterate].
11. [for] looks fine, by the way, why do these libraries have a huge code base?
12. [Abstraction will leak](https://en.wikipedia.org/wiki/Leaky_abstraction). [Keep it simple, stupid](https://en.wikipedia.org/wiki/KISS_principle).

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation

