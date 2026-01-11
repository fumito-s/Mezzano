;;;; Generic SIMD functions and types

(in-package :mezzano.simd)

;;; The SIMD-PACK type is the generic SIMD vector type. It has a fixed element type
;;; and count. Somewhat similar to a 1D simple array, except that it is immutable and
;;; it has no identity (same as numbers & characters).
;;; Element types are restricted to the specific float types, unsigned-byte, and signed-byte.
;;; The size of unsigned-/signed-byte types must be a power of two.
;;; The element count must also be a power of two.
;;;
;;; `(SIMD-PACK [element-type | *] [element-count | *])`
;;;
;;; Architectures define operations that only operate on specific types of simd-pack,
;;; for example (SIMD-PACK SINGLE-FLOAT 4), a 4-element vector of single-floats.

(defconstant +simd-pack-element-type+ (byte 3 0))
(defconstant +simd-pack-element-type-float+ 0)
(defconstant +simd-pack-element-type-unsigned-byte+ 1)
(defconstant +simd-pack-element-type-signed-byte+ 2)

(defconstant +simd-pack-element-width+ (byte 8 3))
(defconstant +simd-pack-element-combined-type/width+ (byte 11 0))

(defconstant +simd-pack-element-count+ (byte 8 15))

(declaim (inline simd-pack-p))
(defun simd-pack-p (object)
  (int::%object-of-type-p object int::+object-tag-simd-pack+))

(defun simd-pack-type-p (object type)
  (destructuring-bind (&optional (element-type '*) (element-count '*))
      (cdr type)
    (and (simd-pack-p object)
         (or (eql element-type '*)
             (int::type-equal (simd-pack-element-type object) element-type))
         (or (eql element-count '*)
             (eql (simd-pack-element-count object) element-count)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pow2p (x)
    "Test if X is a power of two."
    (zerop (logand x (1- x))))

  (defun encode-simd-pack-header (element-type element-count)
    (let ((header 0))
      (assert (pow2p element-count))
      (setf (ldb +simd-pack-element-count+ header) (1- (integer-length element-count)))
      (cond
        ((int::type-equal element-type 'short-float)
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-float+
               (ldb +simd-pack-element-width+ header) 4))
        ((int::type-equal element-type 'single-float)
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-float+
               (ldb +simd-pack-element-width+ header) 5))
        ((int::type-equal element-type 'double-float)
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-float+
               (ldb +simd-pack-element-width+ header) 6))
        ((int::type-equal element-type 'bit)
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) 0))
        ;; TODO: Make this a bit more clever resolving the element-type
        ((int::type-equal element-type '(unsigned-byte 8))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) 3))
        ((int::type-equal element-type '(unsigned-byte 16))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) 4))
        ((int::type-equal element-type '(unsigned-byte 32))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) 5))
        ((int::type-equal element-type '(unsigned-byte 64))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) 6))
        ((int::type-equal element-type '(signed-byte 8))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-signed-byte+
               (ldb +simd-pack-element-width+ header) 3))
        ((int::type-equal element-type '(signed-byte 16))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-signed-byte+
               (ldb +simd-pack-element-width+ header) 4))
        ((int::type-equal element-type '(signed-byte 32))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-signed-byte+
               (ldb +simd-pack-element-width+ header) 5))
        ((int::type-equal element-type '(signed-byte 64))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-signed-byte+
               (ldb +simd-pack-element-width+ header) 6))
        ((and (consp element-type)
              (eql (car element-type) 'unsigned-byte)
              (consp (cdr element-type))
              (integerp (cadr element-type))
              (pow2p (cadr element-type))
              (null (cddr element-type)))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-unsigned-byte+
               (ldb +simd-pack-element-width+ header) (1- (integer-length (cadr element-type)))))
        ((and (consp element-type)
              (eql (car element-type) 'signed-byte)
              (consp (cdr element-type))
              (integerp (cadr element-type))
              (pow2p (cadr element-type))
              (null (cddr element-type)))
         (setf (ldb +simd-pack-element-type+ header) +simd-pack-element-type-signed-byte+
               (ldb +simd-pack-element-width+ header) (1- (integer-length (cadr element-type)))))
        (t
         (error "Unsupported simd element type ~S" element-type)))
      header))

  (defun compile-simd-pack-type (object type)
    (destructuring-bind (&optional (element-type '*) (element-count '*))
        (if (symbolp type)
            '()
            (cdr type))
      ;; This also checks the validity of both values.
      (let ((reference-header (encode-simd-pack-header
                               (if (eql element-type '*)
                                   'single-float
                                   element-type)
                               (if (eql element-count '*)
                                   4
                                   element-count))))
        ;; TODO: Since everything is packed into the header (including object type tag),
        ;; this should boil down to a single load/cmp instruction. But we currently check
        ;; the header payload and type tag separately.
        (cond ((and (eql element-type '*)
                    (eql element-count '*))
               ;; Both universal, simple type tag test.
               `(simd-pack-p ,object))
              ((eql element-type '*)
               ;; Count known.
               `(and (simd-pack-p ,object)
                     (eql (logand (int::%object-header-data ,object)
                                  ,(mask-field +simd-pack-element-count+ -1))
                          ,(mask-field +simd-pack-element-count+
                                       reference-header))))
              ((eql element-count '*)
               ;; Element type known.
               `(and (simd-pack-p ,object)
                     (eql (logand (int::%object-header-data ,object)
                                  ,(mask-field +simd-pack-element-combined-type/width+ -1))
                          ,(mask-field +simd-pack-element-combined-type/width+
                                       reference-header))))
              (t
               ;; Both known.
               `(and (simd-pack-p ,object)
                     (eql (int::%object-header-data ,object) ,reference-header)))))))

  (int::%define-type-symbol 'simd-pack 'simd-pack-p)
  (int::%define-compound-type 'simd-pack 'simd-pack-type-p)
  (int::%define-compound-type-optimizer 'simd-pack 'compile-simd-pack-type))

(defun simd-pack-total-size (simd-pack)
  "Return the total size, in words, of the simd-pack. Don't use, this is for the GC."
  (int::%type-check simd-pack int::+object-tag-simd-pack+ 'simd-pack)
  (let* ((header (int::%object-header-data simd-pack))
         (width (ldb +simd-pack-element-width+ header))
         (count (ldb +simd-pack-element-count+ header))
         ;; This is the length in log2 bits of the vector itself
         (vector-len-log2bits (+ width count))
         ;; We want it in words (log2)
         (vector-len-log2words (max 1 (- vector-len-log2bits 6)))
         ;; But like actually in words
         (vector-len-words (ash 1 vector-len-log2words)))
    ;; Header word plus alignment word
    (+ 2 vector-len-words)))

(defun simd-pack-element-type (simd-pack)
  "Return the element type of the simd-pack."
  (int::%type-check simd-pack int::+object-tag-simd-pack+ 'simd-pack)
  (let* ((header (int::%object-header-data simd-pack))
         (type (ldb +simd-pack-element-type+ header))
         (width (ldb +simd-pack-element-width+ header)))
    (ecase type
      (#.+simd-pack-element-type-float+
       (ecase width
         (4 'short-float)
         (5 'single-float)
         (6 'double-float)))
      (#.+simd-pack-element-type-unsigned-byte+
       `(unsigned-byte ,(ash 1 width)))
      (#.+simd-pack-element-type-signed-byte+
       `(signed-byte ,(ash 1 width))))))

(defun simd-pack-element-count (simd-pack)
  "Return the number of elements in the simd-pack."
  (int::%type-check simd-pack int::+object-tag-simd-pack+ 'simd-pack)
  (let* ((header (int::%object-header-data simd-pack))
         (count (ldb +simd-pack-element-count+ header)))
    (ash 1 count)))

(defun simd-pack-size (simd-pack)
  "Return the total number of bits in the simd-pack."
  (int::%type-check simd-pack int::+object-tag-simd-pack+ 'simd-pack)
  (let* ((header (int::%object-header-data simd-pack))
         (width (ldb +simd-pack-element-width+ header))
         (count (ldb +simd-pack-element-count+ header)))
    (ash 1 (+ width count))))

(defun make-simd-pack (element-type &rest elements)
  (let* ((count (length elements))
         (header (encode-simd-pack-header element-type count))
         (width (ldb +simd-pack-element-width+ header))
         (type (ldb +simd-pack-element-type+ header))
         (size (ceiling (* (ash 1 width) count) 64))
         (simd-pack (mezzano.runtime::%allocate-object
                     int::+object-tag-simd-pack+
                     ;; Pad word, for alignment.
                     header (1+ size) nil)))
    ;; Populate elements.
    (loop
      for index from 0
      for val in elements
      for raw-val = (progn
                      (assert (typep val element-type))
                      (ecase type
                        (#.+simd-pack-element-type-float+
                         (ecase width
                           (4 (int::%short-float-as-integer val))
                           (5 (int::%single-float-as-integer val))
                           (6 (int::%double-float-as-integer val))))
                        (#.+simd-pack-element-type-unsigned-byte+
                         val)
                        (#.+simd-pack-element-type-signed-byte+
                         (logand val (1- (ash 1 (ash 1 width)))))))
      do
         (cond ((eql width 3)
                (setf (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 index)) raw-val))
               ((eql width 4)
                (setf (int::%object-ref-unsigned-byte-16 simd-pack (+ 4 index)) raw-val))
               ((eql width 5)
                (setf (int::%object-ref-unsigned-byte-32 simd-pack (+ 2 index)) raw-val))
               ((eql width 6)
                (setf (int::%object-ref-unsigned-byte-64 simd-pack (+ 1 index)) raw-val))
               ((eql width 2)
                (multiple-value-bind (offset bit)
                    (truncate index 2)
                  (setf (ldb (byte 4 (* bit 4))
                             (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))
                        raw-val)))
               ((eql width 1)
                (multiple-value-bind (offset bit)
                    (truncate index 4)
                  (setf (ldb (byte 2 (* bit 2))
                             (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))
                        raw-val)))
               ((eql width 0)
                (multiple-value-bind (offset bit)
                    (truncate index 8)
                  (setf (ldb (byte 1 (* bit 1))
                             (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))
                        raw-val)))
               ;; Width is greater than a word.
               (t
                (loop
                  with current = raw-val
                  with scale = (truncate (ash 1 width) 64)
                  with offset = (* index scale)
                  for i below scale
                  do (setf (int::%object-ref-unsigned-byte-64 simd-pack (+ 1 offset i))
                           (logand current (1- (ash 1 64))))
                     (setf current (ash current -64))))))
    simd-pack))

(defun simd-pack-element (simd-pack index)
  (int::%type-check simd-pack int::+object-tag-simd-pack+ 'simd-pack)
  (let* ((header (int::%object-header-data simd-pack))
         (type (ldb +simd-pack-element-type+ header))
         (width (ldb +simd-pack-element-width+ header))
         (count (ash 1 (ldb +simd-pack-element-count+ header))))
    (assert (<= 0 index))
    (assert (< index count))
    ;; Read the bits out of the value.
    (let ((raw-element (cond ((eql width 3)
                              (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 index)))
                             ((eql width 4)
                              (int::%object-ref-unsigned-byte-16 simd-pack (+ 4 index)))
                             ((eql width 5)
                              (int::%object-ref-unsigned-byte-32 simd-pack (+ 2 index)))
                             ((eql width 6)
                              (int::%object-ref-unsigned-byte-64 simd-pack (+ 1 index)))
                             ((eql width 2)
                              (multiple-value-bind (offset bit)
                                  (truncate index 2)
                                (ldb (byte 4 (* bit 4))
                                     (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))))
                             ((eql width 1)
                              (multiple-value-bind (offset bit)
                                  (truncate index 4)
                                (ldb (byte 2 (* bit 2))
                                     (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))))
                             ((eql width 0)
                              (multiple-value-bind (offset bit)
                                  (truncate index 8)
                                (ldb (byte 1 (* bit 1))
                                     (int::%object-ref-unsigned-byte-8 simd-pack (+ 8 offset)))))
                             ;; Width is greater than a word.
                             (t
                              (loop
                                with result = 0
                                with scale = (truncate (ash 1 width) 64)
                                with offset = (* index scale)
                                for i below scale
                                do (setf result
                                         (logior (ash result 64)
                                                 (int::%object-ref-unsigned-byte-64 simd-pack (+ 1 offset i))))
                                finally (return result))))))
      (ecase type
        (#.+simd-pack-element-type-float+
         (ecase width
           (4 (int::%integer-as-short-float raw-element))
           (5 (int::%integer-as-single-float raw-element))
           (6 (int::%integer-as-double-float raw-element))))
        (#.+simd-pack-element-type-unsigned-byte+
         raw-element)
        (#.+simd-pack-element-type-signed-byte+
         (int::sign-extend raw-element (ash 1 width)))))))

(defmethod print-object ((instance simd-pack) stream)
  (print-unreadable-object (instance stream)
    (format stream "~S ~S ~S"
            'simd-pack
            (simd-pack-element-type instance)
            (loop for i below (simd-pack-element-count instance)
                  collect (simd-pack-element instance i)))))

(defmethod describe-object ((instance simd-pack) stream)
  (format stream "~S is a simd-pack, with ~D elements of type ~S~%"
          instance
          (simd-pack-element-count instance)
          (simd-pack-element-type instance))
  (dotimes (i (simd-pack-element-count instance))
    (format stream "~3,' D: ~S~%" i (simd-pack-element instance i))))

(defun transmute-simd-pack (simd-pack new-type new-count)
  "Create a new simd-pack by reinterpreting the bits of SIMD-PACK using the new type & count.
The new type & count must be congruent with the existing type, that is the total size
of the new simd-pack must be the same as the old simd-pack."
  (let* ((header (encode-simd-pack-header new-type new-count))
         (width (ldb +simd-pack-element-width+ header))
         (size (ceiling (* (ash 1 width) new-count) 64))
         (new-pack (mezzano.runtime::%allocate-object
                    int::+object-tag-simd-pack+
                    ;; Pad word, for alignment.
                    header (1+ size) nil)))
    (assert (eql (simd-pack-total-size simd-pack) (simd-pack-total-size new-pack)))
    (dotimes (i (1+ size))
      (setf (int::%object-ref-unsigned-byte-64 new-pack i)
            (int::%object-ref-unsigned-byte-64 simd-pack i)))
    new-pack))

(defmethod make-load-form ((object simd-pack) &optional environment)
  (declare (ignore environment))
  `(make-simd-pack ',(simd-pack-element-type object)
                   ,@(loop for i below (simd-pack-element-count object)
                           collect (simd-pack-element object i))))
