;;;; Backend IR extended instruction definitions.

(in-package :mezzano.compiler.backend)

(defclass predicate-instruction (backend-instruction)
  ((%value :initarg :value :accessor predicate-value)
   (%result :initarg :result :accessor predicate-result)))

(defmethod instruction-inputs ((instruction predicate-instruction))
  (list (predicate-value instruction)))

(defmethod instruction-outputs ((instruction predicate-instruction))
  (list (predicate-result instruction)))

(defmethod replace-all-registers ((instruction predicate-instruction) substitution-function)
  (setf (predicate-value instruction) (funcall substitution-function (predicate-value instruction)))
  (setf (predicate-result instruction) (funcall substitution-function (predicate-result instruction))))

(defgeneric lower-predicate-to-call (backend-function predicate-instruction))

(defclass value-has-tag-p-instruction (predicate-instruction)
  ((%tag :initarg :tag :accessor value-has-tag-p-tag)))

(defmethod print-instruction ((instruction value-has-tag-p-instruction))
  (format t "   ~S~%"
          `(:value-has-tag-p-instruction
            ,(predicate-result instruction)
            ,(predicate-value instruction)
            ,(value-has-tag-p-tag instruction))))

(defmethod lower-predicate-to-call (backend-function (instruction value-has-tag-p-instruction))
  (let ((tag (make-instance 'virtual-register)))
    (insert-before backend-function instruction
                   (make-instance 'constant-instruction
                                  :value (value-has-tag-p-tag instruction)
                                  :destination tag))
    (change-class instruction
                  'call-instruction
                  :function 'sys.int::%value-has-tag-p
                  :arguments (list (predicate-value instruction) tag)
                  :result (predicate-result instruction))))

(defclass fixnump-instruction (predicate-instruction)
  ())

(defmethod print-instruction ((instruction fixnump-instruction))
  (format t "   ~S~%"
          `(:fixnump
            ,(predicate-result instruction)
            ,(predicate-value instruction))))

(defmethod lower-predicate-to-call (backend-function (instruction fixnump-instruction))
  (change-class instruction
                'call-instruction
                :function 'sys.int::fixnump
                :arguments (list (predicate-value instruction))
                :result (predicate-result instruction)))
