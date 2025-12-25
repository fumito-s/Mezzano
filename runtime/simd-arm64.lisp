;;;; AdvSIMD support for arm64

(in-package :mezzano.simd.arm64)

;;; Runtime support

;; These are all the possible simd pack types supported by advsimd.
;; They're, to some extent, mutually compatible, in that they can
;; call be reinterpret-cast'd between each other.
(deftype advsimd-pack ()
  `(or (simd:simd-pack single-float 4)
       (simd:simd-pack double-float 2)
       (simd:simd-pack (unsigned-byte 8) 16)
       (simd:simd-pack (unsigned-byte 16) 8)
       (simd:simd-pack (unsigned-byte 32) 4)
       (simd:simd-pack (unsigned-byte 64) 2)
       (simd:simd-pack (signed-byte 8) 16)
       (simd:simd-pack (signed-byte 16) 8)
       (simd:simd-pack (signed-byte 32) 4)
       (simd:simd-pack (signed-byte 64) 2)))

;; These are types expected to be immediates
(deftype imm1 () '(unsigned-byte 1))
(deftype imm2 () '(unsigned-byte 2))
(deftype imm3 () '(unsigned-byte 3))
(deftype imm4 () '(unsigned-byte 4))
(deftype imm5 () '(unsigned-byte 5))
(deftype imm6 () '(unsigned-byte 6))
(deftype imm7 () '(unsigned-byte 7))
(deftype imm8 () '(unsigned-byte 8))

;; Called by the compiler to box vectors.
;; Header data should be boxed fixnum in x1.
(int::define-lap-function %%make-simd-pack-q0 ()
  (:gc :no-frame :layout #*)
  (a64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (a64:add :x29 :sp :xzr)
  (:gc :frame)
  (a64:str :q0 (:pre :sp -16))
  (a64:mov :x5 #.(ash 4 int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (a64:mov :x0 #.(ash int::+object-tag-simd-pack+
                      int::+n-fixnum-bits+))
  ;; Header data already in x1.
  ;; Words.
  (a64:mov :x2 #.(ash 3 int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (a64:mov :x3 :x26)
  ;; Allocate object.
  (a64:named-call mezzano.runtime::%allocate-object)
  ;; Set data.
  (a64:ldr :q0 (:post :sp 16))
  (a64:str :q0 (:object :x0 1))
  ;; Single-value return.
  (a64:mov :x5 #.(ash 1 int::+n-fixnum-bits+)) ; fixnum 1
  (a64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (a64:ret))

;;; Operation definition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun virtual-register-kind-for-type (type-name)
    (ecase type-name
      (f32 :single-float)
      (f64 :double-float)
      ((u8 u16 u32 u64 s8 s16 s32 s64) :integer)
      ((f32.4 f64.2
        u8.16 u16.8 u32.4 u64.2
        s8.16 s16.8 s32.4 s64.2)
       :advsimd)))
  (defun box-instruction-fragment (type-name source destination)
    (let ((inst (ecase type-name
                  (f32 'mezzano.compiler.backend:box-single-float-instruction)
                  (f64 'mezzano.compiler.backend:box-double-float-instruction)
                  ((u8 u16 u32 s8 s16 s32) 'mezzano.compiler.backend:box-fixnum-instruction)
                  (u64 'mezzano.compiler.backend:box-unsigned-byte-64-instruction)
                  (s64 'mezzano.compiler.backend:box-signed-byte-64-instruction)
                  ((f32.4 f64.2
                    u8.16 u16.8 u32.4 u64.2
                    s8.16 s16.8 s32.4 s64.2)
                   'c.a64::box-advsimd-instruction))))
      `(make-instance ',inst
                      :source ,source
                      :destination ,destination
                      ,@(when (eql inst 'c.a64::box-advsimd-instruction)
                          `(:header ,(ecase type-name
                                       (f32.4 (simd::encode-simd-pack-header 'single-float 4))
                                       (f64.2 (simd::encode-simd-pack-header 'double-float 2))
                                       (u8.16 (simd::encode-simd-pack-header '(unsigned-byte 8) 16))
                                       (u16.8 (simd::encode-simd-pack-header '(unsigned-byte 16) 8))
                                       (u32.4 (simd::encode-simd-pack-header '(unsigned-byte 32) 4))
                                       (u64.2 (simd::encode-simd-pack-header '(unsigned-byte 64) 2))
                                       (s8.16 (simd::encode-simd-pack-header '(signed-byte 8) 16))
                                       (s16.8 (simd::encode-simd-pack-header '(signed-byte 16) 8))
                                       (s32.4 (simd::encode-simd-pack-header '(signed-byte 32) 4))
                                       (s64.2 (simd::encode-simd-pack-header '(signed-byte 64) 2))))))))
  (defun unbox-instruction-fragment (type-name source destination)
    `(make-instance ',(ecase type-name
                        (f32 'mezzano.compiler.backend:unbox-single-float-instruction)
                        (f64 'mezzano.compiler.backend:unbox-double-float-instruction)
                        ((u8 u16 u32 s8 s16 s32) 'mezzano.compiler.backend:unbox-fixnum-instruction)
                        (u64 'mezzano.compiler.backend:unbox-unsigned-byte-64-instruction)
                        (s64 'mezzano.compiler.backend:unbox-signed-byte-64-instruction)
                        ((f32.4 f64.2
                          u8.16 u16.8 u32.4 u64.2
                          s8.16 s16.8 s32.4 s64.2)
                         'c.a64::unbox-advsimd-instruction))
                    :source ,source
                    :destination ,destination))
  (defun scalar-promotion (type)
    (case type
      (f32.4 (values 'f32 '%f32.4-broadcast))
      (f64.2 (values 'f64 '%f64.2-broadcast))
      (u8.16 (values 'u8  '%u8.16-broadcast))
      (u16.8 (values 'u16 '%u16.8-broadcast))
      (u32.4 (values 'u32 '%u32.4-broadcast))
      (u64.2 (values 'u64 '%u64.2-broadcast))
      (s8.16 (values 's8  '%s8.16-broadcast))
      (s16.8 (values 's16 '%s16.8-broadcast))
      (s32.4 (values 's32 '%s32.4-broadcast))
      (s64.2 (values 's64 '%s64.2-broadcast))))
  ;; As we generate transform permutations rather than being clever in the transform function,
  ;; we need to make sure the number of arguments stays low (ideally 0, 1, or 2. maybe 3 in a pinch)
  ;; More than that will blow up to a huge number of permutations.
  ;; Unfortunately the types are lost by the time we get to body of the transform.
  (defun generate-permutation-list (types)
    (if types
        (let ((inner (generate-permutation-list (cdr types))))
          (multiple-value-bind (scalar promotor)
              (scalar-promotion (car types))
            (cond (scalar
                   ;; Type needs to be permuted
                   (append (loop for list in inner
                                 collect (list* (list (car types) nil) list))
                           (loop for list in inner
                                 collect (list* (list scalar promotor (car types)) list))))
                  (t
                   ;; Otherwise fine
                   (loop for list in inner
                         collect (list* (list (car types) nil) list))))))
        '(())))
  (defun generate-transform-permutations (name internal-name
                                          result-types value-types
                                          result-names value-names
                                          shiftp)
    (declare (ignorable result-names))
    `(progn
       ,@(loop for permuted-value-types in (generate-permutation-list value-types)
               collect `(c::define-transform ,name (,@(loop for (ty) in permuted-value-types
                                                            for name in value-names
                                                            collect `(,name ,(immediate-actual-type ty shiftp))))
                            ((:optimize (= safety 0) (= speed 3)))
                          `(the (values ,@',result-types)
                                (c::call ,',internal-name
                                         ,,@(loop for name in value-names
                                                  for (nil promotor vector-type) in permuted-value-types
                                                  collect (if promotor
                                                              ``(the ,',vector-type
                                                                     (c::call ,',promotor ,,name))
                                                              name))))))))
  (defun immediatep (type)
    (member type '(imm1 imm2 imm3 imm4 imm5 imm6 imm7 imm8)))
  (defun immediate-max (type)
    (ecase type
      (imm1 2)
      (imm2 4)
      (imm3 8)
      (imm4 16)
      (imm5 32)
      (imm6 64)
      (imm7 128)
      (imm8 256)))
  (defun immediate-actual-type (type shiftp)
    (if (and shiftp (immediatep type))
        (ecase type
          (imm1 `(integer 1 2))
          (imm2 `(integer 1 4))
          (imm3 `(integer 1 8))
          (imm4 `(integer 1 16))
          (imm5 `(integer 1 32))
          (imm6 `(integer 1 64))
          (imm7 `(integer 1 128))
          (imm8 `(integer 1 256)))
        type)))

;; This big scary macro takes in a bunch of names, arguments, types, along with an opcode & operand
;; and produces all the machinery needed to compile that operation efficiently & inline, for most cases.

(defmacro define-op (name result-types value-types result-names value-names opcode operands &key shiftp rmw)
  (let ((boxed-values (loop for name in value-names
                            collect (make-symbol (format nil "~A-BOXED" (string name)))))
        (boxed-results (loop for name in result-names
                             collect (make-symbol (format nil "~A-BOXED" (string name)))))
        (internal-name (intern (format nil "%~A" name)
                               (symbol-package name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(generate-transform-permutations
           name internal-name
           result-types value-types
           result-names value-names
           shiftp)
         (c::mark-as-constant-foldable ',name)
         (c::mark-as-constant-foldable ',internal-name)
         (c.a64::define-builtin ,internal-name (,(loop for boxed-name in boxed-values
                                                       for name in value-names
                                                       for ty in value-types
                                                       collect (if (immediatep ty)
                                                                   `(:constant ,name (typep ,name ',(immediate-actual-type ty shiftp)))
                                                                   boxed-name))
                                                ,boxed-results
                                                :has-wrapper nil)
           (let (,@(loop for ty in value-types
                         for name in value-names
                         unless (immediatep ty)
                         collect `(,name (make-instance
                                          'mezzano.compiler.backend:virtual-register
                                          :kind ,(virtual-register-kind-for-type ty))))
                 ,@(loop for ty in result-types
                         for name in result-names
                         collect `(,name (make-instance
                                          'mezzano.compiler.backend:virtual-register
                                          :kind ,(virtual-register-kind-for-type ty)))))
             ;; Unbox values
             ,@(loop for ty in value-types
                     for unboxed in value-names
                     for boxed in boxed-values
                     unless (immediatep ty)
                     collect `(c.a64::emit
                               ,(unbox-instruction-fragment ty boxed unboxed)))
             (c.a64::emit
              ,(if rmw
                   `(make-instance 'c.a64::arm64-rmw-instruction
                                  :opcode ',opcode
                                  :operands ,operands
                                  :inputs (list ,@value-names)
                                  :outputs (list ,@result-names))
                   `(make-instance 'c.a64::arm64-instruction
                                  :opcode ',opcode
                                  :operands ,operands
                                  :inputs (list ,@value-names)
                                  :outputs (list ,@result-names))))
             ;; Box results
             ,@(loop for ty in result-types
                     for unboxed in result-names
                     for boxed in boxed-results
                     collect `(c.a64::emit
                               ,(box-instruction-fragment ty unboxed boxed))))))
       ;; A wrapper for the internal-name is needed so the compiler can do constant-folding through it.
       (defun ,internal-name ,value-names
         ;; If there are immedates, then we need to switch on them.
         ;; FIXME: Nested switches if there are multiple immediates.
         ,(let* ((immediate-pos (position-if #'immediatep value-types))
                 (imm-ty (and immediate-pos (elt value-types immediate-pos)))
                 (imm-name (and immediate-pos (elt value-names immediate-pos))))
            (if immediate-pos
                `(ecase ,imm-name
                   ,@(loop for i from (if shiftp 1 0) below (+ (immediate-max imm-ty) (if shiftp 1 0))
                           collect `(,i (,internal-name ,@(loop for name in value-names
                                                                collect (if (eql name imm-name)
                                                                            i
                                                                            name))))))
                `(,internal-name ,@value-names))))
       (defun ,name ,value-names
         (,internal-name
         ,@(loop for ty in value-types
                 for name in value-names
                 collect (if (immediatep ty)
                             name
                             `(,ty ,name))))))))

(defmacro %define-aref-transforms (aref row-major-aref %row-major-aref scalar-type vector-type n-lanes setter)
  "Generate wrapper functions and transforms to transform a call to aref/row-major-aref to the appropriate builtin."
  `(progn
     (defun ,aref (,@(when setter '(value)) array &rest subscripts)
       ;; FIXME: Since we're loading N elements out of the array, we should check bounds
       ;; on the last subscript.
       (funcall ',row-major-aref ,@(when setter '(value)) array (apply #'array-row-major-index array subscripts)))
     ;; TODO: Support arbitrary non-simple arrays.
     (defun ,row-major-aref (,@(when setter '(value)) array index)
       (check-type array (simple-array ,scalar-type *))
       (assert (<= 0 index))
       (assert (< (+ index ,n-lanes) (array-total-size array)))
       (funcall ',%row-major-aref
                ,@(when setter '(value))
                (if (typep array '(simple-array * (*)))
                    array ; 1D simple array
                    ;; Otherwise fetch the underlying 1D storage array
                    (int::%object-ref-t array 'int::+complex-array-storage+))
                index))
     ;; TODO: Support other non-1D arrays
     ;; TODO: Broadcast scalar values.
     (c::define-transform ,row-major-aref (,@(when setter `((value ,vector-type)))
                                           (vector (simple-array ,scalar-type (*)) array-type)
                                           (index fixnum index-type))
         ((:optimize (= safety 0) (= speed 3)))
       `(the ,',vector-type
             (progn
               ,(c::insert-bounds-check vector array-type index index-type :adjust ,(1- n-lanes))
               (c::call ,',%row-major-aref ,,@(when setter '(value)) ,vector ,index))))))

;;; Generate aref accessors for the given type.

(defmacro define-aref (aref row-major-aref scalar-type vector-type n-lanes)
  (let ((%row-major-aref
          (intern (format nil "%~A" row-major-aref) (symbol-package row-major-aref))))
    `(progn
       (c::define-aref-transform ,aref ,row-major-aref ,scalar-type)

       (%define-aref-transforms ,aref ,row-major-aref ,%row-major-aref ,scalar-type ,vector-type ,n-lanes nil)
       (%define-aref-transforms (setf ,aref) (setf ,row-major-aref) (setf ,%row-major-aref) ,scalar-type ,vector-type ,n-lanes t)

       (c.a64::define-builtin ,%row-major-aref ((array index) result
                                                :has-wrapper nil)
         (let ((unboxed-result (make-instance
                                'mezzano.compiler.backend:virtual-register
                                :kind :advsimd)))
           (c.a64::with-builtin-object-access (ea ea-inputs array index ,(/ 16 n-lanes))
             (c.a64::emit
              (make-instance 'c.a64::arm64-instruction
                             :opcode 'a64:ldr
                             :operands (list unboxed-result ea)
                             :inputs ea-inputs
                             :outputs (list unboxed-result))))
           (c.a64::emit
            (make-instance 'c.a64::box-advsimd-instruction
                           :source unboxed-result
                           :destination result
                           :header (simd::encode-simd-pack-header ',scalar-type ,n-lanes)))))
       (defun ,%row-major-aref (array index)
         (,%row-major-aref array index))

       (c.a64::define-builtin (setf ,%row-major-aref) ((value array index) result
                                                       :has-wrapper nil)
         (let ((unboxed-value (make-instance
                               'mezzano.compiler.backend:virtual-register
                               :kind :advsimd)))
           (c.a64::emit
            (make-instance 'c.a64::unbox-advsimd-instruction
                           :source value
                           :destination unboxed-value))
           (c.a64::with-builtin-object-access (ea ea-inputs array index ,(/ 16 n-lanes))
             (c.a64::emit
              (make-instance 'c.a64::arm64-instruction
                             :opcode 'a64:str
                             :operands (list unboxed-value ea)
                             :inputs (list* unboxed-value ea-inputs)
                             :outputs '())))
           (c.a64::emit
            (make-instance 'mezzano.compiler.backend:move-instruction
                           :source value
                           :destination result))))
       (defun (setf ,%row-major-aref) (value array index)
         (setf (,%row-major-aref array index) value)))))

;;; Generate a broadcast cast function, either returning a vector of the correct type
;;; unchanged, or a scalar broadcast to a vector.

(defmacro define-broadcast-cast (name broadcast-scalar vector-type scalar-type)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (c::define-transform ,name ((value ,scalar-type))
           ((:optimize (= safety 0) (= speed 3)))
         `(the ,',vector-type
               (c::call ,',broadcast-scalar ,value)))
       (c::define-transform ,name ((value ,vector-type))
           ((:optimize (= safety 0) (= speed 3)))
         `(the ,',vector-type ,value))
       (c::mark-as-constant-foldable ',name))
     (defun ,name (value)
       (etypecase value
         (,scalar-type (,broadcast-scalar value))
         (,vector-type value)))))

(defmacro define-reinterpret-cast (name vector-type scalar-type)
  (let ((internal-scalar-name (intern (format nil "%~A-FROM-~A" name scalar-type)
                                      (symbol-package name)))
        (internal-vector-name (intern (format nil "%~A-FROM-ADVSIMD" name)
                                      (symbol-package name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (c::define-transform ,name ((value ,scalar-type))
             ((:optimize (= safety 0) (= speed 3)))
           `(the ,',vector-type
                 (c::call ,',internal-scalar-name ,value)))
         (c::define-transform ,name ((value advsimd-pack))
             ((:optimize (= safety 0) (= speed 3)))
           `(the ,',vector-type
                 (c::call ,',internal-vector-name ,value)))
         (c::mark-as-constant-foldable ',name)
         (c::mark-as-constant-foldable ',internal-scalar-name)
         (c::mark-as-constant-foldable ',internal-vector-name)
         (c.a64::define-builtin ,internal-scalar-name ((boxed-value) boxed-result
                                                       :has-wrapper nil)
           (let ((value (make-instance
                         'mezzano.compiler.backend:virtual-register
                         :kind ,(virtual-register-kind-for-type scalar-type)))
                 (result (make-instance
                          'mezzano.compiler.backend:virtual-register
                          :kind ,(virtual-register-kind-for-type vector-type))))
             ;; Unbox values
             (c.a64::emit
              ,(unbox-instruction-fragment scalar-type 'boxed-value 'value))
             (c.a64::emit
              (make-instance 'mezzano.compiler.backend:move-instruction
                             :source value
                             :destination result))
             ;; Box results
             (c.a64::emit
              ,(box-instruction-fragment vector-type 'result 'boxed-result))))
         (c.a64::define-builtin ,internal-vector-name ((boxed-value) boxed-result
                                                       :has-wrapper nil)
           (let ((value (make-instance
                         'mezzano.compiler.backend:virtual-register
                         :kind ,(virtual-register-kind-for-type vector-type))))
             ;; Unbox values
             (c.a64::emit
              ,(unbox-instruction-fragment vector-type 'boxed-value 'value))
             ;; Box results
             (c.a64::emit
              ,(box-instruction-fragment vector-type 'value 'boxed-result)))))
       ;; A wrapper for the internal-name is needed so the compiler can do constant-folding through it.
       (defun ,internal-scalar-name (value)
         (,internal-scalar-name value))
       (defun ,internal-vector-name (value)
         (,internal-vector-name value))
       (defun ,name (value)
         (etypecase value
           (,scalar-type (,internal-scalar-name value))
           (advsimd-pack (,internal-vector-name value)))))))

(defmacro define-scalar-cast (name)
  `(progn
     (declaim (inline ,name))
     (defun ,name (value) value)))

(defmacro define-arithmetic-operator (name base identity &key commutative)
  `(progn (defun ,name (&rest numbers)
            (declare (dynamic-extent numbers))
            (let ((result ,identity))
              (dolist (n numbers)
                (setf result (,base result n)))
              result))
          (define-compiler-macro ,name (&rest numbers)
            (cond ((null numbers) ',identity)
                  ((null (rest numbers))
                   `(the number ,(first numbers)))
                  (t (let ((result (first numbers)))
                       (dolist (n (rest numbers))
                         (setf result (list ',base result n)))
                       result))))))

;;; Float vectors

(deftype f32 () `single-float)
(deftype f32.4 () `(simd:simd-pack f32 4))

(define-scalar-cast f32)
(define-broadcast-cast f32.4 f32.4-broadcast f32.4 f32)
(define-reinterpret-cast f32.4! f32.4 f32)

(defun make-f32.4 (x y z w)
  (let* ((t1 (f32.4! (f32 x)))
         (t2 (f32.4-lane-insert t1 y 1))
         (t3 (f32.4-lane-insert t2 z 2)))
    (f32.4-lane-insert t3 w 3)))

(deftype f64 () `double-float)
(deftype f64.2 () `(simd:simd-pack f64 2))

(define-scalar-cast f64)
(define-broadcast-cast f64.2 f64.2-broadcast f64.2 f64)
(define-reinterpret-cast f64.2! f64.2 f64)

(defun make-f64.2 (x y)
  (let* ((t1 (f64.2! (f64 x))))
    (f64.2-lane-insert t1 y 1)))

(defmacro define-float-op (base-name result-types value-types results values opcode operand)
  (flet ((doit (scalar-type vector-type type-keyword)
           (flet ((update-type (type)
                    (case type
                      (scalar scalar-type)
                      (vector vector-type)
                      (t type))))
           (let ((name (intern (format nil "~A~A" vector-type base-name)
                               (symbol-package vector-type))))
             `(define-op ,name
                  ,(mapcar #'update-type result-types)
                  ,(mapcar #'update-type value-types)
                  ,results ,values
                  ,opcode
                  (let ((type-keyword ,type-keyword))
                    ,operand))))))
    `(progn
       ,(doit 'f32 'f32.4 :4s)
       ,(doit 'f64 'f64.2 :2d))))

(define-float-op -broadcast (vector) (scalar)        (result) (value)   a64:dup.v  `(,type-keyword ,result (:fp-128 ,value) 0))
(define-float-op +-two-arg  (vector) (vector vector) (result) (lhs rhs) a64:fadd.v `(,result ,lhs ,rhs ,type-keyword))
(define-float-op --two-arg  (vector) (vector vector) (result) (lhs rhs) a64:fsub.v `(,result ,lhs ,rhs ,type-keyword))

(int::define-commutative-arithmetic-operator f32.4+ f32.4+-two-arg (f32.4 0.0f0))
(int::define-commutative-arithmetic-operator f64.2+ f64.2+-two-arg (f64.2 0.0d0))

(define-aref f32.4-aref f32.4-row-major-aref f32 f32.4 4)
(define-aref f64.2-aref f64.2-row-major-aref f64 f64.2 2)

;;; Integer vectors

(defmacro define-integer-vector (scalar-type vector-type bit-width lane-count signedp)
  (flet ((name (suffix)
           (intern (format nil "~A~A" vector-type suffix) (symbol-package vector-type))))
    (multiple-value-bind (full-width half-width base-width lane-imm shift-imm gpr-width wider)
        (ecase bit-width
          (8  (values :16b :8b :b 'imm4 'imm3 :gp-32 (if signedp 's16.8 'u16.8)))
          (16 (values :8h  :4h :h 'imm3 'imm4 :gp-32 (if signedp 's32.4 'u32.4)))
          (32 (values :4s  :2s :s 'imm2 'imm5 :gp-32 (if signedp 's64.2 'u64.2)))
          (64 (values :2d  nil :d 'imm1 'imm6 :gp-64 nil)))
      `(progn
         (deftype ,scalar-type () '(,(if signedp 'signed-byte 'unsigned-byte) ,bit-width))
         (deftype ,vector-type () '(simd:simd-pack ,scalar-type ,lane-count))

         (define-scalar-cast ,scalar-type)
         (define-broadcast-cast ,vector-type ,(name "-BROADCAST") ,vector-type ,scalar-type)
         (define-reinterpret-cast ,(name "!") ,vector-type ,scalar-type)

         (define-op ,(name "-BROADCAST")           (,vector-type) (,scalar-type)              (result) (value)       a64:dup.v   `(,',full-width ,result (,',gpr-width ,value)))
         (define-op ,(name "-DUP")                 (,vector-type) (,vector-type ,lane-imm)    (result) (value lane)  a64:dup.v   `(,',full-width ,result ,value ,lane))
         (define-op ,(name "-LANE-EXTRACT")        (,scalar-type) (,vector-type ,lane-imm)    (result) (value lane)
             ,(if (and signedp (not (eql bit-width 64))) 'a64:smov.v 'a64:umov.v)
             ,(cond (signedp ``(,',base-width ,result ,value ,lane)) ; force 64-bit dest
                    (t       ``(,',base-width (,',gpr-width ,result) ,value ,lane))))
         (define-op ,(name "-NOT")                 (,vector-type) (,vector-type)              (result) (value)       a64:not.v   `(,result ,value :16b))
         (define-op ,(name "+-TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:add.v   `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "+-SATURATING-TWO-ARG") (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:sqadd.v 'a64:uqadd.v) `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "--TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:sub.v   `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "-SHIFTR")              (,vector-type) (,vector-type ,shift-imm)   (result) (value shift) ,(if signedp 'a64:sshr.v 'a64:ushr.v) `(,result ,value ,shift ,',full-width) :shiftp t)

         ,@(when wider
             `((define-op ,(name "*-LONG")         (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull.v 'a64:umull.v) `(,result ,lhs ,rhs ,',half-width))
               (define-op ,(name "*-LONG-HI")      (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull2.v 'a64:umull2.v) `(,result ,lhs ,rhs ,',full-width))
               (define-op ,(name (format nil "-FROM-~A" wider)) (,vector-type) (,wider)       (result) (value)       a64:xtn.v   `(,result ,value ,',half-width))
               #++(define-op ,(name (format nil "-FROM-~A-HI" wider)) (,vector-type) (,vector-type ,wider) (result) (lhs rhs) a64:xtn2.v `(,result ,lhs ,rhs ,',full-width) :rmw t)))

         (define-arithmetic-operator ,(name "+") ,(name "+-TWO-ARG") (,vector-type 0) :commutative t)
         (define-arithmetic-operator ,(name "+-SATURATING") ,(name "+-SATURATING-TWO-ARG") (,vector-type 0) :commutative t)
         (define-arithmetic-operator ,(name "-") ,(name "--TWO-ARG") (,vector-type 1))

         (define-aref ,(name "-AREF") ,(name "-ROW-MAJOR-AREF") ,scalar-type ,vector-type ,lane-count)))))

(define-integer-vector u8  u8.16 8  16 nil)
(define-integer-vector u16 u16.8 16 8  nil)
(define-integer-vector u32 u32.4 32 4  nil)
(define-integer-vector u64 u64.2 64 2  nil)

(define-integer-vector s8  s8.16 8  16 t)
(define-integer-vector s16 s16.8 16 8  t)
(define-integer-vector s32 s32.4 32 4  t)
(define-integer-vector s64 s64.2 64 2  t)
