(in-package :mezzano.supervisor)

;;; PSCI interface

(sys.int::defglobal *psci-method*)

;; Called late (before FDT scan) to initialize PSCI driver
(defun initialize-psci ()
  (setf *psci-method* nil))

;; Called during late FDT scan to register PSCI method
(defun psci-register (node)
  (let ((method (fdt-get-property node "method")))
    (when method
      (cond ((fdt-string-compare (fdt-property-data method) "smc")
             (setf *psci-method* :smc))
            ((fdt-string-compare (fdt-property-data method) "hvc")
             (setf *psci-method* :hvc)))))
  (debug-print-line "found psci interface at " node " using method " *psci-method*)
  (when *psci-method*
    (debug-print-line "psci version " (psci-version))))

;;; PSCI call methods

(sys.int::define-lap-function %psci-call-hvc ((function-id p1 p2 p3 p4))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:stp :x13 :x14 (:pre :sp -16))
  (:gc :no-frame :layout #*0011)
  (mezzano.lap.arm64:add :x0 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x1 :xzr :x1 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x2 :xzr :x2 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x3 :xzr :x3 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x4 :xzr :x4 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:hvc 0)
  (mezzano.lap.arm64:add :x0 :xzr :x0 :lsl #.sys.int::+n-fixnum-bits+)
  ;; Make sure to clear any value registers, firmware may have clobbered them
  (mezzano.lap.arm64:mov :x1 :xzr)
  (mezzano.lap.arm64:mov :x2 :xzr)
  (mezzano.lap.arm64:mov :x3 :xzr)
  (mezzano.lap.arm64:mov :x4 :xzr)
  (mezzano.lap.arm64:mov :x6 :xzr)
  (mezzano.lap.arm64:mov :x7 :xzr)
  (mezzano.lap.arm64:ldp :x13 :x14 (:post :sp 16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 return value.
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %psci-call-smc ((function-id p1 p2 p3 p4))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:stp :x13 :x14 (:pre :sp -16))
  (:gc :no-frame :layout #*0011)
  (mezzano.lap.arm64:add :x0 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x1 :xzr :x1 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x2 :xzr :x2 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x3 :xzr :x3 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x4 :xzr :x4 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:smc 0)
  (mezzano.lap.arm64:add :x0 :xzr :x0 :lsl #.sys.int::+n-fixnum-bits+)
  ;; Make sure to clear any value registers, firmware may have clobbered them
  (mezzano.lap.arm64:mov :x1 :xzr)
  (mezzano.lap.arm64:mov :x2 :xzr)
  (mezzano.lap.arm64:mov :x3 :xzr)
  (mezzano.lap.arm64:mov :x4 :xzr)
  (mezzano.lap.arm64:mov :x6 :xzr)
  (mezzano.lap.arm64:mov :x7 :xzr)
  (mezzano.lap.arm64:ldp :x13 :x14 (:post :sp 16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 return value.
  (mezzano.lap.arm64:ret))

(defun %psci-call (function-id &optional p1 p2 p3 p4)
  (ecase *psci-method*
    (:hvc (%psci-call-hvc function-id p1 p2 p3 p4))
    (:smc (%psci-call-smc function-id p1 p2 p3 p4))))

;;; PSCI Functions

(defmacro define-psci (name function lambda-list is-64-bit)
  `(defun ,name ,lambda-list
     (%psci-call ,(logior function (if is-64-bit #xC4000000 #x84000000)) ,@lambda-list)))

(define-psci psci-version              0 () nil)
(define-psci psci-cpu-suspend          1 (power-state entry-point-address context-id) t)
(define-psci psci-cpu-off              2 () t)
(define-psci psci-cpu-on               3 (target-cpu entry-point-address context-id) t)
(define-psci psci-affinity-info        4 (target-affinity lowest-affinity-level) t)
(define-psci psci-migrate              5 (target-cpu) t)
(define-psci psci-migrate-info-type    6 () nil)
(define-psci psci-migrate-info-up-cpu  7 () t)
(define-psci psci-system-off           8 () nil)
(define-psci psci-system-reset         9 () nil)
(define-psci psci-features            10 (psci-function-id) nil)
(define-psci psci-cpu-freeze          11 () nil)
(define-psci psci-cpu-default-suspend 12 (entry-point-address context-id) t)
(define-psci psci-node-hw-state       13 (target-cpu power-state) t)
(define-psci psci-system-suspend      14 (entry-point-address context-id) t)
(define-psci psci-set-suspend-mode    15 (mode) t)
(define-psci psci-stat-residency      16 (target-cpu power-state) t)
(define-psci psci-stat-count          17 (target-cpu power-state) t)
