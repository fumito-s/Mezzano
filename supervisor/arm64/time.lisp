(in-package :mezzano.supervisor)

(sys.int::defglobal *generic-timer-rate*)
(sys.int::defglobal *generic-timer-reset-value*)
(sys.int::defglobal *run-time-advance*)

(sys.int::defglobal *rtc-adjust*)

(defun generic-timer-irq-handler (interrupt-frame irq)
  (declare (ignore irq))
  (setf (%cntv-tval-el0) *generic-timer-reset-value*)
  (%isb)
  (beat-heartbeat *run-time-advance*)
  (profile-sample interrupt-frame)
  :completed)

(defun initialize-platform-time (fdt-node)
  (let* ((fdt-interrupt (fdt-get-property fdt-node "interrupts"))
         ;; FIXME: need to deal with PPI vs SPI. 16 is PPI offset.
         ;; Read the IRQ for the virtual timer
         (irq (+ 16 (fdt-read-u32 fdt-interrupt 7)))
         (timer-rate (%cntfrq-el0))
         (tick-rate 100))
    (debug-print-line "Timer irq: " irq)
    (debug-print-line "Timer frequency: " timer-rate " Hz")
    (setf *generic-timer-rate* timer-rate)
    (setf *generic-timer-reset-value* (truncate timer-rate tick-rate))
    (setf *run-time-advance* (truncate internal-time-units-per-second tick-rate))
    (debug-print-line "Timer reset: " *generic-timer-reset-value*)
    (debug-print-line "Timer advance: " *run-time-advance*)
    (when (not (boundp '*rtc-adjust*))
      (setf *rtc-adjust* 0))
    (irq-attach (platform-irq irq)
                #'generic-timer-irq-handler
                fdt-node
                :exclusive t)
    ;; Set countdown value.
    ;; ### why is this 0 and not *generic-timer-reset-value*?
    (setf (%cntv-tval-el0) 0)
    (%isb)
    ;; Enable the timer.
    (setf (%cntv-ctl-el0) 1)
    (%isb)))

(sys.int::defglobal *pl031-rtc-base*)

(defconstant +pl031-rtcdr+ #x00) ; Data register (RO)
(defconstant +pl031-rtcmr+ #x04) ; Match register (RW)
(defconstant +pl031-rtclr+ #x08) ; Load register (RW)
(defconstant +pl031-rtccr+ #x0C) ; Control reigster (RW)
(defconstant +pl031-rtcimsc+ #x10) ; Interrupt Mask Set or Clear register (RW)
(defconstant +pl031-rtcris+ #x14) ; Raw Interrupt Status (RO)
(defconstant +pl031-rtcmis+ #x18) ; Masked Interrupt Status (RO)
(defconstant +pl031-rtcicr+ #x1C) ; Interrupt Clear Register (WO)

(defconstant +pl031-conversion-value+ 2208988800)

(defun pl031-reg (index)
  (physical-memref-unsigned-byte-32 (+ *pl031-rtc-base* index)))

(defun initialize-arm-rtc (fdt-node address-cells size-cells)
  (let* ((reg (fdt-get-property fdt-node "reg"))
         (address (fdt-read-integer reg address-cells 0)))
    (setf *pl031-rtc-base* address)
    (setf *rtc-adjust* (+ +pl031-conversion-value+ (pl031-reg +pl031-rtcdr+)))))

(defun get-universal-time ()
  (+ *rtc-adjust* (truncate (%cntvct-el0) *generic-timer-rate*)))

(defun sys.int::tsc ()
  ;; This isn't the cycle counter, but it's close enough for now.
  (prog1
      (%cntvct-el0)
    (%isb)))

(defun get-high-precision-timer ()
  ;; TODO
  (prog1
      (%cntvct-el0)
    (%isb)))

(defun high-precision-time-units-to-internal-time-units (hp-time)
  (if (boundp '*generic-timer-rate*)
      ;; Do this to avoid producing intermediate bignum or ratio results.
      ;; This loses a bit of precision...
      (truncate hp-time (truncate *generic-timer-rate* internal-time-units-per-second))
      0))
