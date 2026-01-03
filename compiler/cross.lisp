;;;; Packages used for cross-compilation.

;; This is the first file loaded by ASDF.
;;
;; It sets up the CROSS-CL package, which is equivalent to the CL package,
;; except that a bunch of symbols in it are shadowed. As parts of the cross
;; compiler gets loaded we define our own versions of these symbols. This allows
;; the compiler to be loaded in both the cross-environment (on a non-mezzano host
;; lisp), and in the native environment with minimal changes.
;;
;; All other packages depend on this instead of CL, and if the host symbol needs
;; to be referenced, it's explicitly called out using the CL: package prefix.
;;
;; Defpackage in the cross-environment (ie `cross-cl:defpackage`) is modified
;; so that uses of the CL package are changed to uses of CROSS-CL.

(in-package :cl-user)

(defpackage :cross-cl
  (:use :cl)
  (:shadow :defconstant
           :proclaim
           :get-setf-expansion
           :macroexpand
           :macroexpand-1
           :compiler-macro-function
           :macro-function
           :most-positive-fixnum
           :most-negative-fixnum
           :lambda-list-keywords
           :*features*
           :compile
           :*macroexpand-hook*
           :make-load-form
           :*compile-verbose*
           :*compile-print*
           :*compile-file-pathname*
           :*compile-file-truename*
           :compile-file-pathname
           :compile-file
           :constantp
           :array-rank-limit
           :array-dimension-limit
           :array-total-size-limit
           :char-code-limit
           :call-arguments-limit
           :lambda-parameters-limit
           :multiple-values-limit
           :most-negative-short-float
           :most-negative-single-float
           :most-negative-double-float
           :most-negative-long-float
           :most-positive-short-float
           :most-positive-single-float
           :most-positive-double-float
           :most-positive-long-float
           :least-negative-short-float
           :least-negative-single-float
           :least-negative-double-float
           :least-negative-long-float
           :least-positive-short-float
           :least-positive-single-float
           :least-positive-double-float
           :least-positive-long-float
           :least-negative-normalized-short-float
           :least-negative-normalized-single-float
           :least-negative-normalized-double-float
           :least-negative-normalized-long-float
           :least-positive-normalized-short-float
           :least-positive-normalized-single-float
           :least-positive-normalized-double-float
           :least-positive-normalized-long-float
           :short-float-epsilon
           :single-float-epsilon
           :double-float-epsilon
           :long-float-epsilon
           :short-float-negative-epsilon
           :single-float-negative-epsilon
           :double-float-negative-epsilon
           :long-float-negative-epsilon
           :boole-1
           :boole-2
           :boole-andc1
           :boole-andc2
           :boole-and
           :boole-c1
           :boole-c2
           :boole-clr
           :boole-eqv
           :boole-ior
           :boole-nand
           :boole-nor
           :boole-orc1
           :boole-orc2
           :boole-set
           :boole-xor
           :internal-time-units-per-second
           :get-internal-run-time
           :get-internal-real-time
           :pi
           :byte
           :byte-size
           :byte-position
           :ldb
           :dpb
           :mask-field
           :subtypep
           :typep
           :upgraded-array-element-type
           :namestring
           :defpackage
           :fboundp
           :*readtable*
           :*read-base*
           :*read-eval*
           :*read-default-float-format*
           :*read-suppress*
           :read
           :read-preserving-whitespace
           :read-delimited-list
           :readtable
           :readtablep
           :copy-readtable
           :readtable-case
           :get-macro-character
           :set-macro-character
           :make-dispatch-macro-character
           :get-dispatch-macro-character
           :set-dispatch-macro-character
           :set-syntax-from-char
           :with-standard-io-syntax
           :name-char
           :make-hash-table
           :with-compilation-unit
           :upgraded-complex-part-type
           :complex
           :coerce)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

(defpackage :cross-cl-user
  (:use :cross-cl))

(defpackage :mezzano.internals
  (:use :cross-cl))

(defpackage :cross-support
  (:local-nicknames (:sys.int :mezzano.internals))
  (:use :cross-cl))

(defpackage :mezzano.runtime
  (:use :cross-cl))

(declaim (declaration mezzano.internals::lambda-name))

(in-package :cross-support)

(defstruct (byte
             (:constructor byte (size position)))
  size
  position)

;; TODO: Should this do something for (complex short-float)?
(setf (find-class 'complex) (find-class 'cl:complex))

(defmethod make-load-form ((object byte) &optional environment)
  (declare (ignore environment))
  `(byte ',(byte-size object) ',(byte-position object)))

(defstruct cross-short-float value)
(defstruct cross-complex-short-float realpart imagpart)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun loose-constant-equal (x y)
  (or (eql x y)
      (and (cl:typep x 'byte)
           (cl:typep y 'byte)
           (equalp x y))
      (and (cl:typep x 'cross-short-float)
           (cl:typep y 'cross-short-float)
           (eql (cross-short-float-value x)
                (cross-short-float-value y)))))
)

;; Super early definition until the real DEFCONSTANT is loaded.
(defmacro defconstant (name value &optional doc)
  `(alexandria:define-constant ,name ,value
     :test 'loose-constant-equal
     ,@(when doc (list :documentation doc))))
