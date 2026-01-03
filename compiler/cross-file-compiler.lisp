(in-package :mezzano.compiler)

(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)

(defvar sys.int::*top-level-form-number* nil)

(defvar *output-fasl*)
(defvar *output-map*)
(defvar *output-dry-run*)
(defvar *pending-llf-commands*)

(defun x-compile-top-level-implicit-progn (forms env mode)
  (dolist (f forms)
    (x-compile-top-level f env mode)))

(defun x-compile-top-level-lms-body (forms env mode)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (x-compile-top-level-implicit-progn
     body (extend-environment env :declarations declares) mode)))

(defun make-macrolet-env (definitions env)
  (extend-environment env
                      :functions (loop
                         for def in definitions
                                    collect (hack-macrolet-definition def env))))

(defun macroexpand-top-level-form (form env)
  (cond ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) 'sys.int::define-lap-function)
              (listp (third form)))
         ;; Don't expand DEFINE-LAP-FUNCTION.
         (values form nil))
        (t
         ;; Preserve the above behaviour when recursively macroexpanding.
         (multiple-value-bind (expansion expandedp)
             (macroexpand-1 form env)
           (cond (expandedp
                  (values (macroexpand-top-level-form expansion env)
                          t))
                 (t
                  (values expansion nil)))))))

(defun x-compile-top-level (form env &optional (mode :not-compile-time))
  "Cross-compile a top-level form.
3.2.3.1 Processing of Top Level Forms."
  (let ((expansion (macroexpand-top-level-form form env)))
    (cond ((consp expansion)
           (case (first expansion)
             ;; 3. If the form is a progn form, each of its body forms is sequentially
             ;;    processed as a top level form in the same processing mode.
             ((progn)
              (x-compile-top-level-implicit-progn (rest expansion) env mode))
             ;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
             ;;    establishes the appropriate bindings and processes the body forms as
             ;;    top level forms with those bindings in effect in the same processing mode.
             ((locally)
              (x-compile-top-level-lms-body (rest expansion) env mode))
             ((macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-macrolet-env definitions env) mode)))
             ((symbol-macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-symbol-macrolet-env definitions env) mode)))
             ;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
             ((eval-when)
              (destructuring-bind (situation &body body) (rest expansion)
                (multiple-value-bind (compile load eval)
                    (sys.int::parse-eval-when-situation situation)
                  ;; Figure 3-7. EVAL-WHEN processing
                  (cond
                    ;; Process as compile-time-too.
                    ((or (and compile load)
                         (and (not compile) load eval (eql mode :compile-time-too)))
                     (x-compile-top-level-implicit-progn body env :compile-time-too))
                    ;; Process as not-compile-time.
                    ((or (and (not compile) load eval (eql mode :not-compile-time))
                         (and (not compile) load (not eval)))
                     (x-compile-top-level-implicit-progn body env :not-compile-time))
                    ;; Evaluate.
                    ((or (and compile (not load))
                         (and (not compile) (not load) eval (eql mode :compile-time-too)))
                     (x-eval `(progn ,@body) env))
                    ;; Discard.
                    ((or (and (not compile) (not load) eval (eql mode :not-compile-time))
                         (and (not compile) (not load) (not eval)))
                     nil)
                    (t (error "Impossible!"))))))
             ;; 6. Otherwise, the form is a top level form that is not one
             ;;    of the special cases. In compile-time-too mode, the compiler
             ;;    first evaluates the form in the evaluation environment
             ;;    and then minimally compiles it. In not-compile-time mode,
             ;;    the form is simply minimally compiled. All subforms are
             ;;    treated as non-top-level forms.
             (t (when (eql mode :compile-time-too)
                  (x-eval expansion env))
                (when *output-fasl*
                  (x-compile expansion env)))))
          (t (when (eql mode :compile-time-too)
               (x-eval expansion env))
             (when *output-fasl*
               (x-compile expansion env))))))

(defun sys.int::assemble-lap (code &optional name debug-info wired architecture)
  (declare (ignore wired))
  (multiple-value-bind (mc constants fixups symbols gc-data)
      (let ((mezzano.lap:*function-reference-resolver* #'resolve-fref))
        (declare (special mezzano.lap:*function-reference-resolver*)) ; blech.
        (mezzano.lap:perform-assembly-using-target
         (canonicalize-target architecture)
         code
         :base-address 16
         :initial-symbols '((nil . :fixup)
                            (t . :fixup)
                            (:unbound-value . :fixup)
                            (:symbol-binding-cache-sentinel . :fixup)
                            (:layout-instance-header . :fixup))
         :info (list name debug-info)))
    (declare (ignore symbols))
    (make-cross-function :mc mc
                         :constants constants
                         :fixups fixups
                         :gc-info gc-data)))

(defun write-llf-header (output-stream input-file)
  (declare (ignore input-file))
  ;; TODO: write the source file name out as well.
  (write-sequence #(#x4C #x4C #x46 #x01) output-stream)
  (save-integer sys.int::*llf-version* output-stream)
  (save-integer (ecase *target-architecture*
                  (:x86-64 sys.int::+llf-arch-x86-64+)
                  (:arm64 sys.int::+llf-arch-arm64+))
                output-stream))

(defun save-integer (integer stream)
  (let ((negativep (minusp integer)))
    (when negativep (setf integer (- integer)))
    (do ()
        ((zerop (logand integer (lognot #x3F)))
         (write-byte (logior integer (if negativep #x40 0)) stream))
      (write-byte (logior #x80 (logand integer #x7F))
                  stream)
      (setf integer (ash integer -7)))))

;;; FIXME: This should allow saving of all attributes and arbitrary codes.
(defun save-character (character stream)
  (let ((code (char-code character)))
    (assert (zerop (char-bits character)) (character))
    (assert (and (<= 0 code #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
            (character))
    (cond ((<= code #x7F)
           (write-byte code stream))
          ((<= #x80 code #x7FF)
           (write-byte (logior (ash (logand code #x7C0) -6) #xC0) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((or (<= #x800 code #xD7FF)
               (<= #xE000 code #xFFFF))
           (write-byte (logior (ash (logand code #xF000) -12) #xE0) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((<= #x10000 code #x10FFFF)
           (write-byte (logior (ash (logand code #x1C0000) -18) #xF0) stream)
           (write-byte (logior (ash (logand code #x3F000) -12) #x80) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          (t (error "TODO character ~S." character)))))

(defgeneric save-one-object (object object-map stream))

(defmethod save-one-object ((object sys.int::instance-header) omap stream)
  (save-object (sys.int::layout-class (mezzano.runtime::%unpack-instance-header object)) omap stream)
  (write-byte sys.int::+llf-instance-header+ stream))

(defmethod save-one-object ((object cross-fref) omap stream)
  (save-object (cross-fref-name object) omap stream)
  (write-byte sys.int::+llf-function-reference+ stream))

(defmethod save-one-object ((object cross-symbol-global-value-cell) omap stream)
  (save-object (cross-symbol-global-value-cell-name object) omap stream)
  (write-byte sys.int::+llf-symbol-global-value-cell+ stream))

(defmethod save-one-object ((object cross-function) omap stream)
  (let ((constants (cross-function-constants object)))
    (dotimes (i (length constants))
      (save-object (aref constants i) omap stream))
    (save-object (cross-function-fixups object) omap stream)
    (write-byte sys.int::+llf-function+ stream)
    (write-byte sys.int::+object-tag-function+ stream) ; tag, normal function.
    (save-integer (length (cross-function-mc object)) stream)
    (save-integer (length constants) stream)
    (save-integer (length (cross-function-gc-info object)) stream)
    (write-sequence (cross-function-mc object) stream)
    (write-sequence (cross-function-gc-info object) stream)))

(defmethod save-one-object ((object cons) omap stream)
  (cond ((alexandria:proper-list-p object)
         (let ((len 0))
           (dolist (o object)
             (save-object o omap stream)
             (incf len))
           (write-byte sys.int::+llf-proper-list+ stream)
           (save-integer len stream)))
        (t (save-object (cdr object) omap stream)
           (save-object (car object) omap stream)
           (write-byte sys.int::+llf-cons+ stream))))

(defmethod save-one-object ((object symbol) omap stream)
  (cond ((symbol-package object)
         (write-byte sys.int::+llf-symbol+ stream)
         (save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (when (eql package (find-package :cross-cl))
             (setf package (find-package :cl)))
           (save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (save-character (char (package-name package) i) stream))))
        (t
         (save-object (symbol-name object) omap stream)
         (write-byte sys.int::+llf-uninterned-symbol+ stream))))

(defmethod save-one-object ((object string) omap stream)
  (write-byte sys.int::+llf-string+ stream)
  (save-integer (length object) stream)
  (dotimes (i (length object))
    (save-character (char object i) stream)))

(defmethod save-one-object ((object integer) omap stream)
  (write-byte sys.int::+llf-integer+ stream)
  (save-integer object stream))

(defmethod save-one-object ((object vector) omap stream)
  (cond ((every #'integerp object)
         (write-byte sys.int::+llf-integer-vector+ stream)
         (save-integer (length object) stream)
         (dotimes (i (length object))
           (save-integer (aref object i) stream)))
        (t
         (write-byte sys.int::+llf-simple-vector+ stream)
         (save-integer (length object) stream)
         (dotimes (i (length object))
           (save-object (aref object i) omap stream))
         (write-byte sys.int::+llf-initialize-array+ stream)
         (save-integer (length object) stream))))

(defmethod save-one-object ((object character) omap stream)
  (write-byte sys.int::+llf-character+ stream)
  (save-character object stream))

(defmethod save-one-object ((object sys.int::structure-definition) omap stream)
  (save-object (sys.int::structure-definition-name object) omap stream)
  (save-object (sys.int::structure-definition-slots object) omap stream)
  (save-object (sys.int::structure-definition-parent object) omap stream)
  (save-object (sys.int::structure-definition-area object) omap stream)
  (save-object (sys.int::structure-definition-size object) omap stream)
  (save-object (sys.int::layout-heap-layout (sys.int::structure-definition-layout object)) omap stream)
  ;; TODO: Include layout-instance-slots
  (save-object (sys.int::structure-definition-sealed object) omap stream)
  (save-object (sys.int::structure-definition-docstring object) omap stream)
  (save-object (sys.int::structure-definition-has-standard-constructor object) omap stream)
  (write-byte sys.int::+llf-structure-definition+ stream))

(defmethod save-one-object ((object sys.int::layout) omap stream)
  (save-one-object (sys.int::layout-class object) omap stream)
  (write-byte sys.int::+llf-layout+ stream))

(defmethod save-one-object ((object sys.int::structure-slot-definition) omap stream)
  (save-object (sys.int::structure-slot-definition-name object) omap stream)
  (save-object (sys.int::structure-slot-definition-accessor object) omap stream)
  (save-object (sys.int::structure-slot-definition-initform object) omap stream)
  (save-object (sys.int::structure-slot-definition-type object) omap stream)
  (save-object (sys.int::structure-slot-definition-read-only object) omap stream)
  (save-object (sys.int::structure-slot-definition-location object) omap stream)
  (save-object (sys.int::structure-slot-definition-fixed-vector object) omap stream)
  (save-object (sys.int::structure-slot-definition-align object) omap stream)
  (save-object (sys.int::structure-slot-definition-dcas-sibling object) omap stream)
  (save-object (sys.int::structure-slot-definition-documentation object) omap stream)
  (write-byte sys.int::+llf-structure-slot-definition+ stream))

(defmethod save-one-object ((object float) omap stream)
  (etypecase object
    (single-float
     (write-byte sys.int::+llf-single-float+ stream)
     (save-integer (sys.int::%single-float-as-integer object) stream))
    (double-float
     (write-byte sys.int::+llf-double-float+ stream)
     (save-integer (sys.int::%double-float-as-integer object) stream))))

(defmethod save-one-object ((object cross-support::cross-short-float) omap stream)
  (write-byte sys.int::+llf-short-float+ stream)
  (save-integer (sys.int::%short-float-as-integer object) stream))

(defmethod save-one-object ((object ratio) omap stream)
  (write-byte sys.int::+llf-ratio+ stream)
  (save-integer (numerator object) stream)
  (save-integer (denominator object) stream))

(defmethod save-one-object ((object array) omap stream)
  (dotimes (i (array-total-size object))
    (save-object (row-major-aref object i) omap stream))
  (write-byte sys.int::+llf-array+ stream)
  (save-integer (array-rank object) stream)
  (dolist (dim (array-dimensions object))
    (save-integer dim stream)))

(defmethod save-one-object ((object bit-vector) omap stream)
  (write-byte sys.int::+llf-bit-vector+ stream)
  (save-integer (length object) stream)
  (dotimes (i (ceiling (length object) 8))
    (let ((octet 0))
      (dotimes (j 8)
        (let ((idx (+ (* i 8) j)))
          (when (>= idx (length object)) (return))
          (setf (ldb (byte 1 j) octet) (bit object idx))))
      (write-byte octet stream))))

(defmethod save-one-object ((object byte) omap stream)
  (write-byte sys.int::+llf-byte+ stream)
  (save-integer (byte-size object) stream)
  (save-integer (byte-position object) stream))

(defmethod save-one-object ((object complex) omap stream)
  (etypecase (realpart object)
    (rational
     (write-byte sys.int::+llf-complex-rational+ stream)
     (save-integer (numerator (realpart object)) stream)
     (save-integer (denominator (realpart object)) stream)
     (save-integer (numerator (imagpart object)) stream)
     (save-integer (denominator (imagpart object)) stream))
    (single-float
     (write-byte sys.int::+llf-complex-single-float+ stream)
     (save-integer (sys.int::%single-float-as-integer (realpart object)) stream)
     (save-integer (sys.int::%single-float-as-integer (imagpart object)) stream))
    (double-float
     (write-byte sys.int::+llf-complex-double-float+ stream)
     (save-integer (sys.int::%double-float-as-integer (realpart object)) stream)
     (save-integer (sys.int::%double-float-as-integer (imagpart object)) stream))))

(defmethod save-one-object ((object cross-support::cross-complex-short-float) omap stream)
  (write-byte sys.int::+llf-complex-short-float+ stream)
  (save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-realpart object)) stream)
  (save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-imagpart object)) stream))

(defun save-object (object omap stream)
  (let ((info (alexandria:ensure-gethash object omap (list (hash-table-count omap) 0 nil))))
    (cond (*output-dry-run*
           (incf (second info))
           (when (eql (second info) 1)
             (save-one-object object omap stream)))
          (t (when (not (third info))
               (save-one-object object omap stream)
               (setf (third info) t)
               (unless (eql (second info) 1)
                 (write-byte sys.int::+llf-add-backlink+ stream)
                 (save-integer (first info) stream)))
             (unless (eql (second info) 1)
                 (write-byte sys.int::+llf-backlink+ stream)
                 (save-integer (first info) stream))))))

(defun add-to-llf (action &rest objects)
  (push (list* action objects) *pending-llf-commands*))

(defvar *failed-fastload-by-symbol* (make-hash-table))

(defun cross-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  (let ((ltv-sym (gensym "LOAD-TIME-VALUE-CELL")))
    (x-compile `(locally
                    (declare (special ,ltv-sym))
                  (setq ,ltv-sym ,form))
               nil)
    `(sys.int::symbol-global-value ',ltv-sym)))

;; One of:
;;   'symbol
;;   #'symbol
;;   #'(SETF symbol)
;;   #'(CAS symbol)
(defun valid-funcall-function-p (form)
  (and (consp form)
       (consp (cdr form))
       (null (cddr form))
       (or (and (eql (first form) 'quote)
                (symbolp (second form)))
           (and (eql (first form) 'function)
                (let ((name (second form)))
                  (or (symbolp name)
                      (and (consp name)
                           (consp (cdr name))
                           (null (cddr name))
                           (member (first name) '(setf sys.int::cas))
                           (symbolp (second name)))))))))

;; Convert a valid funcall function to the function name.
(defun funcall-function-name (form)
  (second form))

(defun x-compile (form env)
  (cond
    ;; Special case (define-lap-function name (options...) code...)
    ;; Don't macroexpand this, as it expands into a bunch of difficult to
    ;; recognize nonsense.
    ((and (consp form)
          (eql (first form) 'sys.int::define-lap-function)
          (>= (length form) 3))
     (destructuring-bind (name (&optional lambda-list frame-layout environment-vector-offset environment-vector-layout) &body code)
         (cdr form)
       (let ((docstring nil))
         (when (stringp (first code))
           (setf docstring (pop code)))
         (x-compile
          `(sys.int::%defun ',name
                            ',(sys.int::assemble-lap
                               code
                               name
                               (list :debug-info
                                     name
                                     frame-layout
                                     environment-vector-offset
                                     environment-vector-layout
                                     (when *compile-file-pathname*
                                       (princ-to-string *compile-file-pathname*))
                                     sys.int::*top-level-form-number*
                                     lambda-list
                                     docstring
                                     nil)
                               nil
                               *target-architecture*))
          env))))
    (t
     (x-compile-for-value form env)
     (add-to-llf sys.int::+llf-drop+))))

(defun x-compile-for-value (form env)
  ;; FIXME: This should probably use compiler-macroexpand.
  (let ((expansion (macroexpand form env)))
    (cond
      ((symbolp expansion)
       (if (or (keywordp expansion) (member expansion '(nil t)))
           (add-to-llf nil expansion)
           (add-to-llf sys.int::+llf-funcall-n+ expansion 'symbol-value 1)))
      ((not (consp expansion))
       ;; Self-evaluating form.
       (add-to-llf nil expansion))
      ((eql (first expansion) 'quote)
       (add-to-llf nil (second expansion)))
      ((and (eql (first expansion) 'function)
            (consp (second expansion))
            (eql (first (second expansion)) 'lambda))
       (let* ((*load-time-value-hook* 'cross-load-time-value)
              (fn (compile-lambda (second expansion) env *target-architecture*)))
         (declare (special *load-time-value-hook*))
         (add-to-llf nil fn)))
      ((eql (first expansion) 'function)
       (x-compile-for-value `(fdefinition ',(second form)) env))
      ((eql (first expansion) 'setq)
       (x-compile-for-value `(funcall #'(setf symbol-value)
                                      ,(third form)
                                      ',(second form))
                            env))
      ((eql (first expansion) 'if)
       (destructuring-bind (test then &optional else)
           (rest expansion)
         (x-compile-for-value test env)
         (add-to-llf sys.int::+llf-if+)
         (x-compile-for-value then env)
         (add-to-llf sys.int::+llf-else+)
         (x-compile-for-value else env)
         (add-to-llf sys.int::+llf-fi+)))
      ((and (eql (first expansion) 'progn)
            (cdr expansion)
            (endp (cddr expansion)))
       ;; Only (progn foo) supported currently.
       (x-compile-for-value (second expansion) env))
      ((special-operator-p (first expansion))
       ;; Can't convert this, convert it to a zero-argument function and
       ;; call that. PROGN to avoid problems with DECLARE.
       (incf (gethash (first expansion) *failed-fastload-by-symbol* 0))
       (let* ((*load-time-value-hook* 'cross-load-time-value)
              (fn (compile-lambda `(lambda ()
                                     (declare (sys.int::lambda-name
                                               (sys.int::toplevel ,(when *compile-file-pathname*
                                                                         (princ-to-string *compile-file-pathname*))
                                                                  ,sys.int::*top-level-form-number*)))
                                     (progn ,expansion))
                                  env
                                  *target-architecture*)))
         (declare (special *load-time-value-hook*))
         (add-to-llf sys.int::+llf-funcall-n+ fn 0)))
      (t
       ;; That should just leave ordinary calls.
       (let ((name (first expansion))
             (args (rest expansion)))
         ;; Unpeel funcall forms.
         (loop
            (cond ((and (eql name 'funcall)
                        (consp args)
                        (valid-funcall-function-p (first args)))
                   (setf name (funcall-function-name (first args))
                         args (rest args)))
                  (t
                   (return))))
         (dolist (arg args)
           (x-compile-for-value arg env))
         (add-to-llf sys.int::+llf-funcall-n+ name (length args)))))))

(defun cross-compile-file (input-file
                           &key
                             (output-file (make-pathname :type "llf" :defaults input-file))
                             (verbose *compile-verbose*)
                             (print *compile-print*)
                             (external-format :default)
                             package)
  (with-open-file (input input-file :external-format external-format)
    (with-open-file (*output-fasl* output-file
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
      (write-llf-header *output-fasl* input-file)
      (let* ((*readtable* (copy-readtable *readtable*))
             (*output-map* (make-hash-table))
             (*pending-llf-commands* nil)
             (*package* (or (find-package (or package "CROSS-CL-USER"))
                            (error "Unknown package ~S" package)))
             (*compile-print* print)
             (*compile-verbose* verbose)
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (truename *compile-file-pathname*))
             (*gensym-counter* 0)
             (cl:*features* *features*)
             (sys.int::*top-level-form-number* 0)
             (location-stream (make-instance 'sys.int::location-tracking-stream
                                             :stream input
                                             :namestring (namestring *compile-file-pathname*))))
        (when *compile-verbose*
          (format t ";; Cross-compiling ~S~%" input-file))
        (sys.int::with-reader-location-tracking
          (loop
             for form = (read location-stream nil input)
             until (eql form input)
             do
               (when *compile-print*
                 (let ((*print-length* 3)
                       (*print-level* 2))
                   (format t ";; X-compiling: ~S~%" form)))
               (x-compile-top-level form nil)
               (incf sys.int::*top-level-form-number*)))
        ;; Now write everything to the fasl.
        ;; Do two passes to detect circularity.
        (let ((commands (reverse *pending-llf-commands*)))
          (let ((*output-dry-run* t))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* (make-broadcast-stream)))))
          (let ((*output-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* *output-fasl*))
              (when (car cmd)
                (write-byte (car cmd) *output-fasl*)))))
        (write-byte sys.int::+llf-end-of-load+ *output-fasl*))))
  output-file)

(defun load-for-cross-compiler (input-file &key
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (let* ((*readtable* (copy-readtable *readtable*))
           (*package* (find-package "CROSS-CL-USER"))
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*output-fasl* nil)
           (*gensym-counter* 0))
      (when *compile-verbose*
        (format t ";; Cross-loading ~S~%" input-file))
      (loop for form = (read input nil input) do
           (when (eql form input)
             (return))
           (when *compile-print*
             (let ((*print-length* 3)
                   (*print-level* 2))
               (format t ";; X-loading: ~S~%" form)))
           (x-compile-top-level form nil :not-compile-time))))
  t)

(defun save-custom-compiled-file (path generator)
  (with-open-file (*output-fasl* path
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede
                   :direction :output)
    (write-llf-header *output-fasl* path)
    (let* ((*readtable* (copy-readtable *readtable*))
           (*output-map* (make-hash-table))
           (*pending-llf-commands* nil)
           (*package* (find-package "CROSS-CL-USER"))
           (*compile-print* *compile-print*)
           (*compile-verbose* *compile-verbose*)
           (*compile-file-pathname* nil)
           (*compile-file-truename* nil)
           (*gensym-counter* 0))
      (loop
         for values = (multiple-value-list (funcall generator))
         for form = (first values)
         do
           (when (not values)
             (return))
           (when *compile-print*
             (let ((*print-length* 3)
                   (*print-level* 2))
               (format t ";; X-compiling: ~S~%" form)))
           (x-compile-top-level form nil))
      ;; Now write everything to the fasl.
      ;; Do two passes to detect circularity.
      (let ((commands (reverse *pending-llf-commands*)))
        (let ((*output-dry-run* t))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* (make-broadcast-stream)))))
        (let ((*output-dry-run* nil))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* *output-fasl*))
            (when (car cmd)
              (write-byte (car cmd) *output-fasl*)))))
      (write-byte sys.int::+llf-end-of-load+ *output-fasl*))))

(defun save-compiler-builtins (path target-architecture)
  (format t ";; Writing compiler builtins to ~A.~%" path)
  (let* ((builtins (ecase target-architecture
                     (:x86-64 (mezzano.compiler.backend.x86-64::generate-builtin-functions))
                     (:arm64 (mezzano.compiler.backend.arm64::generate-builtin-functions))))
         (*target-architecture* target-architecture))
    (save-custom-compiled-file path
                               (lambda ()
                                 (if builtins
                                     (let ((b (pop builtins)))
                                       `(sys.int::%defun ',(first b) ,(second b)))
                                     (values))))))
