;;;; AdvSIMD support for arm64.
;;;; This file contains the specific vector intrisic definitions.

(in-package :mezzano.simd.arm64)

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
         (define-op ,(name "-AND-TWO-ARG")         (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:and.v   `(,result ,lhs ,rhs :16b))
         (define-op ,(name "+-TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:add.v   `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "+-SATURATING-TWO-ARG") (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:sqadd.v 'a64:uqadd.v) `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "--TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:sub.v   `(,result ,lhs ,rhs ,',full-width))
         (define-op ,(name "-SHIFTR")              (,vector-type) (,vector-type ,shift-imm)   (result) (value shift) ,(if signedp 'a64:sshr.v 'a64:ushr.v) `(,result ,value ,shift ,',full-width) :shiftp t)

         ,@(when wider
             `((define-op ,(name "*-LONG")         (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull.v 'a64:umull.v) `(,result ,lhs ,rhs ,',half-width))
               (define-op ,(name "*-LONG-HI")      (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull2.v 'a64:umull2.v) `(,result ,lhs ,rhs ,',full-width))
               (define-op ,(name (format nil "-FROM-~A" wider)) (,vector-type) (,wider)       (result) (value)       a64:xtn.v   `(,result ,value ,',half-width))
               (define-op ,(name (format nil "-FROM-~A-HI" wider)) (,vector-type) (,vector-type ,wider) (result) (lhs rhs) a64:xtn2.v `(,result ,rhs ,',full-width) :rmw t)))

         (define-arithmetic-operator ,(name "+") ,(name "+-TWO-ARG") (,vector-type 0) :commutative t)
         (define-arithmetic-operator ,(name "+-SATURATING") ,(name "+-SATURATING-TWO-ARG") (,vector-type 0) :commutative t)
         (define-arithmetic-operator ,(name "-") ,(name "--TWO-ARG") (,vector-type 1))
         ,@(unless (eql bit-width 64)
             `((define-op ,(name "*-TWO-ARG")      (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:mul.v   `(,result ,lhs ,rhs ,',full-width))
               (define-arithmetic-operator ,(name "*") ,(name "*-TWO-ARG") (,vector-type 1) :commutative t)))
         (define-arithmetic-operator ,(name "-AND") ,(name "-AND-TWO-ARG") (,vector-type ,(if signedp -1 (1- (ash 1 bit-width)))) :commutative t)

         (define-aref ,(name "-AREF") ,(name "-ROW-MAJOR-AREF") ,scalar-type ,vector-type ,lane-count)))))

(define-integer-vector u8  u8.16 8  16 nil)
(define-integer-vector u16 u16.8 16 8  nil)
(define-integer-vector u32 u32.4 32 4  nil)
(define-integer-vector u64 u64.2 64 2  nil)

(define-integer-vector s8  s8.16 8  16 t)
(define-integer-vector s16 s16.8 16 8  t)
(define-integer-vector s32 s32.4 32 4  t)
(define-integer-vector s64 s64.2 64 2  t)

(c::define-aref-transform u8.16-aref-4-interleaved-in-u32
  u8.16-row-major-aref-4-interleaved-in-u32 u32
  :value-count 4
  :setter-name set-u8.16-aref-4-interleaved-in-u32
  :row-major-setter-name set-u8.16-row-major-aref-4-interleaved-in-u32)

(%define-aref-transforms
 u8.16-aref-4-interleaved-in-u32
 u8.16-row-major-aref-4-interleaved-in-u32
 %u8.16-row-major-aref-4-interleaved-in-u32
 u32 u8.16 4 4 nil)

(%define-aref-transforms
 set-u8.16-aref-4-interleaved-in-u32
 set-u8.16-row-major-aref-4-interleaved-in-u32
 %set-u8.16-row-major-aref-4-interleaved-in-u32
 u32 u8.16 4 4 t)

(defsetf u8.16-aref-4-interleaved-in-u32 (array &rest subscripts) (v1 v2 v3 v4)
  `(set-u8.16-aref-4-interleaved-in-u32 ,v1 ,v2 ,v3 ,v4 ,array ,@subscripts))

(defsetf u8.16-row-major-aref-4-interleaved-in-u32 (array index) (v1 v2 v3 v4)
  `(set-u8.16-row-major-aref-4-interleaved-in-u32 ,v1 ,v2 ,v3 ,v4 ,array ,index))

(c.a64::define-builtin %u8.16-row-major-aref-4-interleaved-in-u32
    ((array index) (r1 r2 r3 r4) :has-wrapper nil)
  (let ((unboxed-r1 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-r2 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-r3 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-r4 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd)))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source array
                    :destination :x1))
    (c.a64::emit
     (make-instance 'c.a64::arm64-ld/st-multiple-instruction
                    :opcode 'a64:ld4
                    :size :4s
                    :direction :load
                    :registers (list unboxed-r1 unboxed-r2 unboxed-r3 unboxed-r4)
                    :index index
                    :scale 4))
    (c.a64::emit
     (make-instance 'c.a64::box-advsimd-instruction
                    :source unboxed-r1
                    :destination r1
                    :header (simd::encode-simd-pack-header 'u8 16)))
    (c.a64::emit
     (make-instance 'c.a64::box-advsimd-instruction
                    :source unboxed-r2
                    :destination r2
                    :header (simd::encode-simd-pack-header 'u8 16)))
    (c.a64::emit
     (make-instance 'c.a64::box-advsimd-instruction
                    :source unboxed-r3
                    :destination r3
                    :header (simd::encode-simd-pack-header 'u8 16)))
    (c.a64::emit
     (make-instance 'c.a64::box-advsimd-instruction
                    :source unboxed-r4
                    :destination r4
                    :header (simd::encode-simd-pack-header 'u8 16)))))

(defun %u8.16-row-major-aref-4-interleaved-in-u32 (array index)
  (%u8.16-row-major-aref-4-interleaved-in-u32 array index))

(c.a64::define-builtin %set-u8.16-row-major-aref-4-interleaved-in-u32
    ((v1 v2 v3 v4 array index) (r1 r2 r3 r4) :has-wrapper nil)
  (let ((unboxed-v1 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-v2 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-v3 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd))
        (unboxed-v4 (make-instance
                     'mezzano.compiler.backend:virtual-register
                     :kind :advsimd)))
    (c.a64::emit
     (make-instance 'c.a64::unbox-advsimd-instruction
                    :source v1
                    :destination unboxed-v1))
    (c.a64::emit
     (make-instance 'c.a64::unbox-advsimd-instruction
                    :source v2
                    :destination unboxed-v2))
    (c.a64::emit
     (make-instance 'c.a64::unbox-advsimd-instruction
                    :source v3
                    :destination unboxed-v3))
    (c.a64::emit
     (make-instance 'c.a64::unbox-advsimd-instruction
                    :source v4
                    :destination unboxed-v4))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source array
                    :destination :x1))
    (c.a64::emit
     (make-instance 'c.a64::arm64-ld/st-multiple-instruction
                    :opcode 'a64:st4
                    :size :4s
                    :direction :store
                    :registers (list unboxed-v1 unboxed-v2 unboxed-v3 unboxed-v4)
                    :index index
                    :scale 4))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source v1
                    :destination r1))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source v2
                    :destination r2))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source v3
                    :destination r3))
    (c.a64::emit
     (make-instance 'mezzano.compiler.backend:move-instruction
                    :source v4
                    :destination r4))))

(defun %set-u8.16-row-major-aref-4-interleaved-in-u32 (v1 v2 v3 v4 array index)
  (%set-u8.16-row-major-aref-4-interleaved-in-u32 v1 v2 v3 v4 array index))
