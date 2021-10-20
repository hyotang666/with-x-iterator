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
| [cl-iterative]       | 17               | Yes         | Yes      |
| [cl-tertools]        | 36               | Yes         | Yes      |
| [iterator-protocol]  | 5                | Yes         | Yes      |
| [doplus]             | 50               | Yes         | No       |
| [picl]               | 31               | Yes         | Yes      |
| [trivial-do]         | 6                | No          | No       |
| with-x-iterator      | 6                | Yes         | Yes      |

[series]: http://series.sourceforge.net/
[iterate]: https://gitlab.common-lisp.net/iterate/iterate
[for]: https://github.com/Shinmera/for
[cl-enumeration]: https://gitlab.common-lisp.net/cl-enumeration/enumerations
[cl-iterative]: https://github.com/mobius-eng/cl-iterative
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
  (do+ ((char (generator "hoge")))
    ()
    (collect char)))
=> (#\h #\o #\g #\e)
```

For usability, a generic-function `generator` is provided.
`list`, `vector`, `sequence`, `hash-table`, and `integer` is supported.

```lisp
(uiop:while-collecting (collect)
  (do+ ((i (generator 5)))
    ()
    (collect i)))
=> (0 1 2 3 4)
```

In fact, with-x-iterator is designed as protocol-based.
Any generator function is acceptable.

For minimize the learning cost, the generator functions protocols are designed as `(FUNCTION () (VALUES Existp Key Value))`.
You know this API is inherited from `cl:with-hash-table-iterator`.

To access the `Key` value, `do+` accepts two vars as its bindings.

```lisp
(uiop:while-collecting (ks vs)
  (do+ (((k v) (generator (alexandria:plist-hash-table '(:a :b :c :d)))))
    ()
    (ks k)
    (vs v)))
=> (:A :C)
   (:B :D)
```

Example of a generator to generate random value.
```lisp
(uiop:while-collecting (acc)
  (do+ ((random (lambda () (values t t (random 10))))
        (times (generator 5)))
    ()
    (declare (ignore times))
    (acc random)))
```

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation

