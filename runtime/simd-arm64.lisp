;;;; AdvSIMD support for arm64

(in-package :mezzano.simd.arm64)

;;; Runtime support

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
