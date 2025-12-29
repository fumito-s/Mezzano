(in-package :mezzano.supervisor)

(sys.int::defglobal *arm64-exception-vector*)
(sys.int::defglobal *arm64-exception-vector-base*)
(sys.int::defglobal *bsp-wired-stack*)
(sys.int::defglobal *bsp-cpu*)

(sys.int::defglobal *n-up-cpus* 1)
(sys.int::defglobal *cpus*)

(defstruct (arm64-cpu
            (:area :wired)
            :slot-locations)
  self
  (state :offline :type (member :offline :online :timed-out))
  cpu-id
  idle-thread
  wired-stack
  (sp-el1 0)
  page-fault-hook)

(defun initialize-boot-cpu ()
  (setf (arm64-cpu-self *bsp-cpu*) *bsp-cpu*)
  (setf (arm64-cpu-state *bsp-cpu*) :online)
  (setf (arm64-cpu-idle-thread *bsp-cpu*)
        sys.int::*bsp-idle-thread*)
  (setf (arm64-cpu-wired-stack *bsp-cpu*) *bsp-wired-stack*)
  (setf (arm64-cpu-sp-el1 *bsp-cpu*)
        (+ (car *bsp-wired-stack*) (cdr *bsp-wired-stack*) -16))
  (setf (sys.int::memref-unsigned-byte-64 (arm64-cpu-sp-el1 *bsp-cpu*))
        (sys.int::lisp-object-address *bsp-cpu*))
  (%load-cpu-bits (arm64-cpu-sp-el1 *bsp-cpu*)
                  (ash (arm64-cpu-sp-el1 *bsp-cpu*) -1)
                  *arm64-exception-vector-base*)
  (setf *cpus* '()))

(defconstant +spsr-ss+ 21)

(defconstant +mdscr-ss+ 0)
(defconstant +mdscr-kde+ 13)
(defconstant +mdscr-mde+ 15)

(sys.int::define-lap-function %load-cpu-bits ((sp-el1 cpu-data vbar-el1))
  (:gc :no-frame :layout #*)
  ;; Switch to SP_EL1.
  (mezzano.lap.arm64:msr :spsel 1)
  ;; Unbox sp-el1.
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  ;; Set SP_EL1.
  (mezzano.lap.arm64:add :sp :x9 0)
  ;; Move back to SP_EL0.
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Set the current CPU register.
  (mezzano.lap.arm64:orr :x27 :xzr :x1)
  ;; Set VBAR_EL1.
  (mezzano.lap.arm64:add :x9 :xzr :x2 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :vbar-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Configure MDSCR_EL1, enable KDE, & MDE. Delay enabling SS until we actually
  ;; want to single-step something.
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-kde+))
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-mde+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function local-cpu-info (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:orr :x9 :xzr :x27)
  (mezzano.lap.arm64:ldr :x0 (:x9))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun local-cpu ()
  (local-cpu-info))

(defun initialize-cpu ()
  (setf (arm64-cpu-cpu-id *bsp-cpu*) (fdt-boot-cpuid))
  (push-wired *bsp-cpu* *cpus*))

(sys.int::define-lap-function %el0-common ()
  ;; Stack looks like:
  ;; +40 pad (ss on x86-64)
  ;; +32 sp (not set)
  ;; +24 spsr (not set)
  ;; +16 x30 (cs on x86-64)
  ;; +8 pc (not set)
  ;; +0 x29 (frame pointer)
  ;; x29 contains function to branch to.
  ;; Push registers in the same order as x86-64.
  (mezzano.lap.arm64:stp :x5 :x9 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x6 :x10 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x12 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x1 :x0 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x3 :x2 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x7 :x4 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x14 :x13 (:pre :sp -16))
  ;; Flush the pad slot.
  (mezzano.lap.arm64:str :xzr (:sp #x98))
  ;; Read & save SP_EL0
  (mezzano.lap.arm64:mrs :x9 :sp-el0)
  (mezzano.lap.arm64:str :x9 (:sp #x90))
  ;; Read & save ELR_EL1
  (mezzano.lap.arm64:mrs :x9 :elr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x78))
  ;; Read & save SPSR_EL1
  (mezzano.lap.arm64:mrs :x9 :spsr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x88))
  ;; Save x30.
  (mezzano.lap.arm64:str :x30 (:sp #x80))
  ;; Clear MDSCR_EL1.SS
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:bfc :x9 #.+mdscr-ss+ 1)
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Set up for call to handler.
  (mezzano.lap.arm64:orr :x7 :xzr :x29)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 arg.
  ;; Build frame.
  (mezzano.lap.arm64:add :x29 :sp #x70)
  ;; Build interrupt frame object.
  (mezzano.lap.arm64:sub :sp :sp 16)
  (mezzano.lap.arm64:movz :x9 #.(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+))
  (mezzano.lap.arm64:str :x9 (:sp))
  (mezzano.lap.arm64:add :x9 :xzr :x29 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:str :x9 (:sp 8))
  (mezzano.lap.arm64:add :x0 :sp #.sys.int::+tag-object+)
  (:gc :frame :interrupt t)
  ;; Call handler.
  ;; Read the function out of the fref.
  (mezzano.lap.arm64:ldr :x6 (:object :x7 #.sys.int::+fref-function+))
  ;; Read the function entry point and call it.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  ;; Drop the frame.
  (mezzano.lap.arm64:add :sp :sp 16)
  ;; Restore x30.
  (mezzano.lap.arm64:ldr :x30 (:sp #x80))
  ;; Restore SPSR_EL1
  (mezzano.lap.arm64:ldr :x9 (:sp #x88))
  (mezzano.lap.arm64:msr :spsr-el1 :x9)
  ;; Enable MDSCR.SS if we're single-stepping.
  (mezzano.lap.arm64:tbz :x9 #.+spsr-ss+ L1)
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-ss+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  L1
  ;; Restore ELR_EL1
  (mezzano.lap.arm64:ldr :x9 (:sp #x78))
  (mezzano.lap.arm64:msr :elr-el1 :x9)
  ;; Restore SP_EL0
  (mezzano.lap.arm64:ldr :x9 (:sp #x90))
  (mezzano.lap.arm64:msr :sp-el0 :x9)
  ;; Restore registers.
  (mezzano.lap.arm64:ldp :x14 :x13 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x7 :x4 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x3 :x2 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x1 :x0 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x12 :x11 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x6 :x10 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x5 :x9 (:post :sp 16))
  (mezzano.lap.arm64:ldr :x29 (:sp))
  (mezzano.lap.arm64:add :sp :sp #x30)
  (mezzano.lap.arm64:eret))

(sys.int::define-lap-function %elx-common ()
  ;; Stack looks like:
  ;; +40 pad (ss on x86-64)
  ;; +32 sp (not set)
  ;; +24 spsr (not set)
  ;; +16 x30 (cs on x86-64)
  ;; +8 pc (not set)
  ;; +0 x29 (frame pointer)
  ;; x29 contains function to branch to.
  ;; Push registers in the same order as x86-64.
  (mezzano.lap.arm64:stp :x5 :x9 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x6 :x10 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x12 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x1 :x0 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x3 :x2 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x7 :x4 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x14 :x13 (:pre :sp -16))
  ;; Flush the pad slot.
  (mezzano.lap.arm64:str :xzr (:sp #x98))
  ;; Read & save SP.
  (mezzano.lap.arm64:add :x9 :sp 0)
  (mezzano.lap.arm64:str :x9 (:sp #x90))
  ;; Read & save ELR_EL1
  (mezzano.lap.arm64:mrs :x9 :elr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x78))
  ;; Read & save SPSR_EL1
  (mezzano.lap.arm64:mrs :x9 :spsr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x88))
  ;; Save x30.
  (mezzano.lap.arm64:str :x30 (:sp #x80))
  ;; Clear MDSCR_EL1.SS
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:bfc :x9 #.+mdscr-ss+ 1)
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Set up for call to handler.
  (mezzano.lap.arm64:orr :x7 :xzr :x29)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 arg.
  ;; Build frame.
  (mezzano.lap.arm64:add :x29 :sp #x70)
  ;; Build interrupt frame object.
  (mezzano.lap.arm64:sub :sp :sp 16)
  (mezzano.lap.arm64:movz :x9 #.(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+))
  (mezzano.lap.arm64:str :x9 (:sp))
  (mezzano.lap.arm64:add :x9 :xzr :x29 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:str :x9 (:sp 8))
  (mezzano.lap.arm64:add :x0 :sp #.sys.int::+tag-object+)
  (:gc :frame :interrupt t)
  ;; Call handler.
  ;; Read the function out of the fref.
  (mezzano.lap.arm64:ldr :x6 (:object :x7 #.sys.int::+fref-function+))
  ;; Read the function entry point and call it.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 4))

(defun broadcast-panic-ipi ()
  nil)

(defun broadcast-wakeup-ipi ()
  nil)

(defun quiesce-cpus-for-world-stop ()
  nil)

(defun begin-tlb-shootdown ()
  nil)

(defun tlb-shootdown-single (address)
  (declare (ignore address))
  nil)

(defun tlb-shootdown-range (base length)
  (declare (ignore base length))
  nil)

(defun tlb-shootdown-all ()
  nil)

(defun finish-tlb-shootdown ()
  nil)

(defun check-tlb-shootdown-not-in-progress ()
  nil)

(defun local-cpu-idle-thread ()
  (arm64-cpu-idle-thread (local-cpu-info)))

(defun register-secondary-cpu (cpu-id)
  (let* ((idle-thread (make-ephemeral-thread #'idle-thread :runnable
                                             :name "Idle Thread"
                                             :priority :idle))
         (wired-stack (%allocate-stack (* 128 1024) t))
         (cpu (make-arm64-cpu :state :offline
                              :cpu-id cpu-id
                              :idle-thread idle-thread
                              :wired-stack wired-stack
                              :sp-el1 (+ (stack-base wired-stack)
                                         (stack-size wired-stack)))))
    (setf (arm64-cpu-self cpu) cpu)
    (debug-print-line "Registered new CPU " cpu " " idle-thread " with ID " cpu-id)
    (push-wired cpu *cpus*)))

(defun detect-secondary-cpus ()
  (let* ((boot-cpu (fdt-boot-cpuid))
         (cpus (fdt-get-named-child-node (fdt-root) "cpus")))
    (debug-print-line "Boot cpu is " boot-cpu)
    (debug-print-line "cpus: " cpus)
    (when cpus
      (do-fdt-child-nodes (node cpus)
        (when (or (fdt-compatible-p node "arm,arm-v8")
                  (fdt-compatible-p node "arm,cortex-a57"))
          (let ((id (fdt-read-u32 (fdt-get-property node "reg"))))
            (when (not (eql id boot-cpu))
              (register-secondary-cpu id))))))))

(defun boot-secondary-cpus ()
  (detect-secondary-cpus)
  nil)

(defun logical-core-count ()
  1)

(defun preemption-timer-reset (time-remaining)
  (declare (ignore time-remaining))
  nil)

(defun preemption-timer-remaining ()
  nil)

(defun stop-other-cpus-for-debug-magic-button ()
  nil)

(defun resume-other-cpus-for-debug-magic-button ()
  nil)

(defun arch-pre-panic ()
  nil)

(sys.int::define-lap-function %dmb.oshld (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:dmb.oshst)
  (mezzano.lap.arm64:ret))

(defun sys.int::dma-write-barrier ()
  (%dmb.oshld)
  (%isb))

(defun restore-page-fault-ist (state)
  (declare (ignore state))
  nil)

(sys.int::define-lap-function %dc.cvau ((address))
  "Clean data cache for the given virtual address back to the point of unification"
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:dc.cvau :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %ic.ivau ((address))
  "Invalidate instruction cache for the given virtual address back to the point of unification"
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ic.ivau :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %dsb.ish (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:dsb.ish)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %isb (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(defun %arm64-sync-icache (start length)
  (let ((end (+ start length)))
    ;; Clear (write dirty data, but don't invalidate) data cache back to
    ;; the point of unification (where I & D caches meet)
    (loop for addr from start below end by 64
          do (%dc.cvau addr))
    ;; Ensure visibility of the data cleaned from cache.
    (%dsb.ish)
    ;; Now that the dcache is up to date at the PoU, any lines in
    ;; the icache can be invalidated back there.
    (loop for addr from start below end by 64
          do (%ic.ivau addr))
    ;; Ensure completion of the invalidations.
    (%dsb.ish)
    ;; Make sure we don't have stale instructions in the pipeline.
    (%isb)))

(defmacro define-system-register-accessors ()
  `(progn
     ,@(loop for (name) in mezzano.lap.arm64::*system-registers*
             for symbol = (intern (format nil "%~A" name))
             collect `(sys.int::define-lap-function ,symbol (())
                        (:gc :no-frame :layout #*)
                        (mezzano.lap.arm64:mrs :x9 ,name)
                        (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
                        (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
                        (mezzano.lap.arm64:ret))
             collect `(sys.int::define-lap-function (setf ,symbol) ((value))
                        (:gc :no-frame :layout #*)
                        (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
                        (mezzano.lap.arm64:msr ,name :x9)
                        (mezzano.lap.arm64:ret)))))

(define-system-register-accessors)
