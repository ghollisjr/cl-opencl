(in-package :cl-opencl)

;; utility macros
(defmacro check-opencl-error (err cleanup &body body)
  "Macro for automating error management with two modes:

1. NULL err denoting error code will be returned by body.

2. Symbol err-address to denote symbol to use for error address so as
to interact with OpenCL CFFI functions.

If cleanup is non-NIL, it must be a function called before signaling
an error.  Useful for preventing memory leaks."
  (cond
    ((null err)
     (let ((err (gensym)))
       `(let* ((,err
                (progn ,@body)))
          (when (not (= ,err +CL-SUCCESS+))
            ,@(when cleanup
                `((funcall ,cleanup)))
            (error "OpenCL Error ~a" ,err))
          ,err)))
    ((symbolp err)
     (let* ((err-address err)
            (err (gensym))
            (return-value (gensym)))
       `(with-foreign-object (,err-address 'cl-int)
          (symbol-macrolet ((,err (mem-ref ,err-address 'cl-int)))
            (let* ((,return-value
                    (progn ,@body)))
              (when (not (= ,err +CL-SUCCESS+))
                ,@(when cleanup
                    `((funcall ,cleanup)))
                (error "OpenCL Error ~a" ,err))
              ,return-value)))))))

(defmacro case= (term &body forms)
  "This is in response to issues I currently do not understand when
trying to use CFFI groveler constant values in a case statement.
Instead of repetitive cond = tests, I use this macro like a case
statement."
  `(cond
     ,@(loop
          for f in forms
          for first = (first f)
          for body = (rest f)
          collecting `((= ,first ,term)
                       ,@body))))

;; Platform API
(defun cl-get-platform-ids ()
  (with-foreign-object (nplatforms :uint)
    (clgetplatformids 0 +NULL+ nplatforms)
    (let* ((nplats (mem-ref nplatforms :uint)))
      (with-foreign-object (platform-ids 'cl-platform-id nplats)
        (check-opencl-error () ()
          (clgetplatformids (mem-ref nplatforms :uint)
                            platform-ids
                            +NULL+))
        (loop
           for i below nplats
           collecting (mem-aref platform-ids
                                'cl-platform-id
                                i))))))

(defun cl-get-platform-info (platform param)
  "Returns platform information."
  (with-foreign-object (size 'size-t)
    (check-opencl-error () ()
      (clGetPlatformInfo platform param
                         0 +NULL+
                         size))
    (with-foreign-object (pp :char (mem-ref size 'size-t))
      (check-opencl-error () ()
        (clGetPlatformInfo platform param
                           (mem-ref size 'size-t)
                           pp
                           +NULL+))
      (foreign-string-to-lisp pp))))

;; Device API
(defun cl-get-device-ids (platform-id platform-type)
  (with-foreign-object (ndevices :uint)
    (clgetdeviceids platform-id platform-type 0 +NULL+ ndevices)
    (let* ((nplats (mem-ref ndevices :uint)))
      (with-foreign-object (device-ids 'cl-device-id nplats)
        (check-opencl-error () ()
          (clgetdeviceids platform-id
                          platform-type
                          (mem-ref ndevices :uint)
                          device-ids
                          cl-opencl-cffi:+NULL+))
        (loop
           for i below nplats
           collecting (mem-aref device-ids 'cl-device-id i))))))

(defun cl-get-device-info (device param)
  "Returns platform information."
  (with-foreign-object (size 'size-t)
    (check-opencl-error () ()
      (clGetDeviceInfo device param
                       0 +NULL+ size))
    (with-foreign-object (p :char (mem-ref size 'size-t))
      (check-opencl-error () ()
        (clGetDeviceInfo device param (mem-ref size 'size-t)
                         p +NULL+))
      (cond
        ((= param +CL-DEVICE-ADDRESS-BITS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-AVAILABLE+)
         (mem-ref p 'cl-bool))
        ((= param +CL-DEVICE-COMPILER-AVAILABLE+)
         (mem-ref p 'cl-bool))
        ((= param +CL-DEVICE-DOUBLE-FP-CONFIG+)
         (mem-ref p 'cl-device-fp-config))
        ((= param +CL-DEVICE-ENDIAN-LITTLE+)
         (mem-ref p 'cl-bool))
        ((= param +CL-DEVICE-ERROR-CORRECTION-SUPPORT+)
         (mem-ref p 'cl-bool))
        ((= param +CL-DEVICE-EXECUTION-CAPABILITIES+)
         (mem-ref p 'cl-device-exec-capabilities))
        ((= param +CL-DEVICE-EXTENSIONS+)
         (foreign-string-to-lisp p))
        ((= param +CL-DEVICE-GLOBAL-MEM-CACHE-SIZE+)
         (mem-ref p 'cl-ulong))
        ((= param +CL-DEVICE-GLOBAL-MEM-CACHE-TYPE+)
         (mem-ref p 'cl-device-mem-cache-type))
        ((= param +CL-DEVICE-GLOBAL-MEM-CACHELINE-SIZE+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-GLOBAL-MEM-SIZE+)
         (mem-ref p 'cl-ulong))
        ((= param +CL-DEVICE-HALF-FP-CONFIG+)
         (mem-ref p 'cl-device-fp-config))
        ((= param +CL-DEVICE-IMAGE-SUPPORT+)
         (mem-ref p 'cl-bool))
        ((= param +CL-DEVICE-IMAGE2D-MAX-HEIGHT+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-IMAGE2D-MAX-WIDTH+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-IMAGE3D-MAX-DEPTH+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-IMAGE3D-MAX-HEIGHT+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-IMAGE3D-MAX-WIDTH+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-LOCAL-MEM-SIZE+)
         (mem-ref p 'cl-ulong))
        ((= param +CL-DEVICE-LOCAL-MEM-TYPE+)
         (mem-ref p 'cl-device-local-mem-type))
        ((= param +CL-DEVICE-MAX-CLOCK-FREQUENCY+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-COMPUTE-UNITS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-CONSTANT-ARGS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-CONSTANT-BUFFER-SIZE+)
         (mem-ref p 'cl-ulong))
        ((= param +CL-DEVICE-MAX-MEM-ALLOC-SIZE+)
         (mem-ref p 'cl-ulong))
        ((= param +CL-DEVICE-MAX-PARAMETER-SIZE+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-MAX-READ-IMAGE-ARGS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-SAMPLERS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-WORK-GROUP-SIZE+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-MAX-WORK-ITEM-DIMENSIONS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MAX-WORK-ITEM-SIZES+)
         (let* ((nitems (floor (mem-ref p 'cl-uint)
                               (foreign-type-size 'size-t))))
           (loop
              for i below nitems
              collecting (mem-aref p 'size-t i))))
        ((= param +CL-DEVICE-MAX-WRITE-IMAGE-ARGS+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MEM-BASE-ADDR-ALIGN+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-MIN-DATA-TYPE-ALIGN-SIZE+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-NAME+)
         (foreign-string-to-lisp p))
        ((= param +CL-DEVICE-PLATFORM+)
         (mem-ref p 'cl-platform-id))
        ((or (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-CHAR+)
             (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-SHORT+)
             (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-INT+)
             (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-LONG+)
             (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-FLOAT+)
             (= param
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-DOUBLE+))
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-PROFILE+)
         (foreign-string-to-lisp p))
        ((= param +CL-DEVICE-PROFILING-TIMER-RESOLUTION+)
         (mem-ref p 'size-t))
        ((= param +CL-DEVICE-QUEUE-PROPERTIES+)
         (mem-ref p 'cl-command-queue-properties))
        ((= param +CL-DEVICE-SINGLE-FP-CONFIG+)
         (mem-ref p 'cl-device-fp-config))
        ((= param +CL-DEVICE-TYPE+)
         (mem-ref p 'cl-device-type))
        ((= param +CL-DEVICE-VENDOR+)
         (foreign-string-to-lisp p))
        ((= param +CL-DEVICE-VENDOR-ID+)
         (mem-ref p 'cl-uint))
        ((= param +CL-DEVICE-VERSION+)
         (foreign-string-to-lisp p))
        ((= param +CL-DRIVER-VERSION+)
         (foreign-string-to-lisp p))))))

;; NOTE: I haven't been able to fully test this function as my system
;; seems unable to create sub-devices at all.
(defun cl-create-sub-devices (device
                              &key
                                equally
                                by-counts
                                by-affinity)
  "Creates num-devices sub-devices from existing device given the
supplied properties list.  Must set exactly one of equally, by-counts,
or by-affinity.

The value of equally is the number of partitions to equally-divide the
compute device into.

The value of by-counts is a list of compute unit counts for each
sub-device created, one element per sub-device.

The value of by-affinity must be one of :numa, :L1, :L2, :L3, :L4, or
:next, denoting division of a device into units sharing NUMA code,
L1/2/3/4 cache, or division into the next smaller level of
memory (NUMA,L4,L3,L2,L1).  cl-get-device-info with the parameter
+CL-DEVICE-PARTITION-TYPE+ can be used on a sub-device to determine
what level of partitioning occurred.

In all cases, a list of device IDs will be returned."
  (cond
    (equally
     (with-foreign-object (properties 'cl-uint 3)
       (setf (mem-aref properties 'cl-uint 0)
             +CL-DEVICE-PARTITION-EQUALLY+)
       (setf (mem-aref properties 'cl-uint 1)
             equally)
       (setf (mem-aref properties 'cl-uint 2)
             0)
       (with-foreign-object (num-devices-ret 'cl-uint)
         (check-opencl-error () ()
           (clCreateSubDevices device properties 0 +NULL+
                               num-devices-ret))
         (with-foreign-object (devices 'cl-device-id
                                       (mem-ref num-devices-ret 'cl-uint))
           (check-opencl-error () ()
             (clCreateSubDevices device properties
                                 (mem-ref num-devices-ret 'cl-uint)
                                 devices +NULL+))
           (loop
              for i below (mem-ref num-devices-ret 'cl-uint)
              collecting (mem-aref devices 'cl-device-id i))))))
    (by-counts
     (with-foreign-object (properties 'cl-uint (+ 2 (length by-counts)))
       (let ((index 0))
         (symbol-macrolet ((next-index (1- (incf index))))
           (setf (mem-aref properties 'cl-uint next-index)
                 +CL-DEVICE-PARTITION-BY-COUNTS+)
           (loop
              for c in by-counts
              do (setf (mem-aref properties 'cl-uint next-index)
                       c))
           (setf (mem-aref properties 'cl-uint next-index)
                 0)))
       (with-foreign-object (num-devices-ret 'cl-uint
                                             (length by-counts))
         (check-opencl-error () ()
           (clCreateSubDevices device properties
                               0 +NULL+
                               num-devices-ret))
         (with-foreign-object (devices 'cl-device-id
                                       (mem-ref num-devices-ret
                                                'cl-uint))
           (check-opencl-error () ()
             (clCreateSubDevices device properties
                                 (mem-ref num-devices-ret 'cl-uint)
                                 devices
                                 +NULL+))
           (loop
              for i below (mem-ref num-devices-ret 'cl-uint)
              collecting (mem-aref devices 'cl-device-id i))))))
    (by-affinity
     (let* ((type
             (cond
               ((eq by-affinity :NUMA)
                +CL-DEVICE-AFFINITY-DOMAIN-NUMA+)
               ((eq by-affinity :L1)
                +CL-DEVICE-AFFINITY-DOMAIN-L1-CACHE+)
               ((eq by-affinity :L2)
                +CL-DEVICE-AFFINITY-DOMAIN-L2-CACHE+)
               ((eq by-affinity :L3)
                +CL-DEVICE-AFFINITY-DOMAIN-L3-CACHE+)
               ((eq by-affinity :L4)
                +CL-DEVICE-AFFINITY-DOMAIN-L4-CACHE+)
               ((eq by-affinity :NEXT)
                +CL-DEVICE-AFFINITY-DOMAIN-NEXT-PARTITIONABLE+)
               (t
                (error "Partition affinity must be one of :numa, :L1, :L2, :L3, :L4, or :NEXT")))))
       (with-foreign-object (properties 'cl-device-affinity-domain
                                        3)
         (setf (mem-aref properties 'cl-device-affinity-domain 0)
               +CL-DEVICE-PARTITION-BY-AFFINITY-DOMAIN+)
         (setf (mem-aref properties 'cl-device-affinity-domain 1)
               type)
         (setf (mem-aref properties 'cl-device-affinity-domain 2)
               0)
         (with-foreign-object (num-devices-ret 'cl-uint)
           (check-opencl-error () ()
             (clCreateSubDevices device properties 0 +NULL+ num-devices-ret))
           (with-foreign-object (devices 'cl-device-id
                                         (mem-ref num-devices-ret
                                                  'cl-uint))
             (check-opencl-error () ()
               (clCreateSubDevices device properties
                                   (mem-ref num-devices-ret
                                            'cl-uint)
                                   devices
                                   +NULL+))
             (loop
                for i below (mem-ref num-devices-ret
                                     'cl-uint)
                collecting (mem-aref devices 'cl-device-id
                                     i)))))))
    (t
     (error "Must set one of equally, by-couts, or by-affinity"))))

(defun cl-retain-device (device)
  (check-opencl-error () ()
    (clRetainDevice device)))

(defun cl-release-device (device)
  (check-opencl-error () ()
    (clReleaseDevice device)))

(defun cl-set-default-device-command-queue (context device queue)
  (check-opencl-error () ()
    (clSetDefaultDeviceCommandQueue context device queue)))

(defun cl-get-device-and-host-timer (device)
  "Returns list of (device-timestamp host-timestamp)."
  (with-foreign-objects ((devstamp 'cl-ulong)
                         (hoststamp 'cl-ulong))
    (check-opencl-error () ()
      (clGetDeviceAndHostTimer device devstamp hoststamp))
    (list (mem-ref devstamp 'cl-ulong)
          (mem-ref hoststamp 'cl-ulong))))

(defun cl-get-host-timer (device)
  "Returns timestamp for host."
  (with-foreign-object (hoststamp 'cl-ulong)
    (check-opencl-error () ()
      (clGetHostTimer device hoststamp))
    (mem-ref hoststamp 'cl-ulong)))

;; Context API
(defun cl-create-context (platform devices
                          &key
                            callback
                            user-data
                            properties)
  "Creates context from platform and list of devices.  Currently
properties isn't used as OpenCL does not support any properties aside
from the platform ID already specified as another argument, but it's
kept here for future purposes."
  (declare (ignore properties)) ; up to OpenCL 2.2 at least
  (let* ((ndevices (length devices)))
    (with-foreign-objects ((devices-pointer 'cl-device-id ndevices)
                           (props 'cl-context-properties 3))
      (loop
         for i from 0
         for did in devices
         do (setf (mem-aref devices-pointer 'cl-device-id i)
                  did))
      (setf (mem-aref props 'cl-context-properties
                      0)
            +CL-CONTEXT-PLATFORM+)
      (setf (mem-aref props 'cl-context-properties
                      1)
            platform)
      (setf (mem-aref props 'cl-context-properties
                      2)
            0)
      (check-opencl-error err ()
        (CLCREATECONTEXT props 1 devices-pointer
                         (if callback
                             callback
                             +NULL+)
                         (if user-data
                             user-data
                             +NULL+)
                         err)))))

(defun cl-create-context-from-type (platform device-type
                                    &key
                                      properties
                                      callback
                                      user-data)
  "Creates context from platform and type of device rather than device
ID."
  (declare (ignore properties)) ; up to OpenCL 2.2 at least
  (with-foreign-objects ((props 'cl-context-properties 3))
    (setf (mem-aref props 'cl-context-properties
                    0)
          +CL-CONTEXT-PLATFORM+)
    (setf (mem-aref props 'cl-context-properties
                    1)
          platform)
    (setf (mem-aref props 'cl-context-properties
                    2)
          0)
    (check-opencl-error err ()
      (clCreateContextFromType props device-type
                               (if callback
                                   callback
                                   +NULL+)
                               (if user-data
                                   user-data
                                   +NULL+)
                               err))))

(defun cl-release-context (context)
  (check-opencl-error () ()
    (CLRELEASECONTEXT context)))

(defun cl-retain-context (context)
  (check-opencl-error () ()
    (CLRETAINCONTEXT context)))

(defmacro with-opencl-context (context platform devices
                               &body body)
  "Macro to automate OpenCL context creation and release."
  (let* ((retval (gensym)))
    `(let* ((,context
             (cl-create-context ,platform ,devices))
            (,retval
             (progn
               ,@body)))
       (cl-release-context ,context)
       ,retval)))

;; Queue API
(defun cl-create-command-queue (context device
                                &key
                                  queue-size
                                  properties)
  "Uses clCreateCommandQueueWithProperties to create command queue
with optional properties set.  If queue-size is non-NIL, then the
+CL-QUEUE-ON-DEVICE+ bitfield will automatically be set in the
properties list.  The properties list should be a list of integer
constants as parsed by the groveler."
  (if (not (or queue-size properties))
      (check-opencl-error err ()
        (clCreateCommandQueueWithProperties context device +NULL+ err))
      (let* ((n-total-props (+ (if properties 1 0)
                               (if queue-size 1 0)))
             (index 0))
        (with-foreign-object (props 'cl-queue-properties
                                    (+ (* 2 n-total-props)
                                       1))
          (symbol-macrolet ((next-property
                             (mem-aref props
                                       'cl-queue-properties
                                       (1- (incf index)))))
            (when queue-size
              (setf next-property
                    +CL-QUEUE-SIZE+)
              (setf next-property
                    queue-size))
            (when properties
              (let* ((bitfield (apply #'logior
                                      properties)))
                (setf next-property
                      +CL-QUEUE-PROPERTIES+)
                (setf next-property
                      bitfield)))
            (setf next-property 0))
          (check-opencl-error err ()
            (clCreateCommandQueueWithProperties context
                                                device
                                                properties
                                                err))))))

(defun cl-retain-command-queue (queue)
  (check-opencl-error () ()
    (clRetainCommandQueue queue)))

(defun cl-release-command-queue (queue)
  (check-opencl-error () ()
    (clReleaseCommandQueue queue)))

(defun cl-get-command-queue-info (queue param)
  (let* ((type
          (case= param
            (+CL-QUEUE-CONTEXT+ 'cl-context)
            (+CL-QUEUE-DEVICE+ 'cl-device-id)
            (+CL-QUEUE-REFERENCE-COUNT+ 'cl-uint)
            (+CL-QUEUE-PROPERTIES+
             'cl-command-queue-properties))))
    (with-foreign-object (result type)
      (check-opencl-error () ()
        (clGetCommandQueueInfo queue param
                               (foreign-type-size type)
                               result
                               +NULL+))
      (mem-ref result type))))

(defmacro with-opencl-command-queue (context device qvar &body body)
  "Macro to automate queue creation and release.  If qvar is a symbol,
then default properties will be used.  If it is list (qvar &key
queue-size properties), then the key args will be supplied to
cl-create-command-queue."
  (let* ((ctext (gensym))
         (dev (gensym))
         (retval (gensym))
         (queue nil)
         (key-args nil))
    (if (symbolp qvar)
        (setf queue qvar)
        (progn
          (setf queue (first qvar))
          (setf key-args (rest qvar))))
    `(let* ((,ctext ,context)
            (,dev ,device)
            (,queue (cl-create-command-queue ,ctext ,dev
                                             ,@key-args))
            (,retval
             (progn
               ,@body)))
       (cl-release-command-queue ,queue)
       ,retval)))

;; Memory Object APIs
(defun join-flags (flags)
  (if (atom flags)
      flags
      (apply #'logior flags)))

(defun cl-create-buffer (context flags
                         &key
                           size
                           type
                           data)
  "Creates buffer with two main modes of operation:

1. Size-based.
2. Data-based.

For size-based, set size to the number of bytes to allocate in the
buffer.  Note that data is ignored when size is specified as it can be
calculated when data is supplied.  For data-based, set type and data
to the foreign type and a list of data to place into the buffer.  Note
that this means initializing the buffer with data requires the
data-based option.

Only one of these should be used, and the size-based mode takes
precedence.  If neither are used, then an error is thrown.

flags should be a list of flags to join with logior or a single
integer."
  (when (not (or size
                 (and type data)))
    (error "Must set either size or type and data to non-NIL values."))
  (let* ((mode (join-flags flags)))
    (if size
        (check-opencl-error err ()
          (clCreateBuffer context
                          mode
                          size
                          +NULL+
                          err))
        (let* ((ndata (length data))
               (size (* (foreign-type-size type)
                        ndata)))
          (with-foreign-object (buf type ndata)
            (loop
               for i below ndata
               for d in data
               do (setf (mem-aref buf type i)
                        d))
            (check-opencl-error err ()
              (clCreateBuffer context
                              mode
                              size
                              buf
                              err)))))))

(defun cl-create-sub-buffer (buffer flags origin size)
  (let* ((bufcreatetype +CL-BUFFER-CREATE-TYPE-REGION+)
         (flags (join-flags flags)))
    (with-foreign-object (bufinfo '(:struct cl-buffer-region))
      (setf (foreign-slot-value bufinfo '(:struct cl-buffer-region)
                                :origin)
            origin)
      (setf (foreign-slot-value bufinfo '(:struct cl-buffer-region)
                                :size)
            size)
      (check-opencl-error err ()
        (clCreateSubBuffer buffer flags
                           bufcreatetype
                           bufinfo
                           err)))))

(defun image-channel-type->data-type (channel-data-type)
  (case= channel-data-type
    (+CL-SNORM-INT8+
     :char)
    (+CL-SNORM-INT16+
     :short)
    (+CL-UNORM-INT8+
     :uchar)
    (+CL-UNORM-INT16+
     :ushort)
    (+CL-UNORM-SHORT-565+
     :ushort)
    (+CL-UNORM-SHORT-555+
     :ushort)
    (+CL-UNORM-INT-101010+
     :uint)
    (+CL-SIGNED-INT8+
     :char)
    (+CL-SIGNED-INT16+
     :short)
    (+CL-SIGNED-INT32+
     :int)
    (+CL-UNSIGNED-INT8+
     :uchar)
    (+CL-UNSIGNED-INT16+
     :ushort)
    (+CL-UNSIGNED-INT32+
     :uint)
    (+CL-HALF-FLOAT+
     'cl-half)
    (+CL-FLOAT+
     :float)))

(defun cl-create-image (context flags
                        &key
                          (image-type +CL-MEM-OBJECT-IMAGE2D+)
                          (image-channel-order +CL-RGBA+)
                          (image-channel-data-type +CL-UNSIGNED-INT8+)
                          (width 1)
                          (height 1)
                          (depth 1)
                          (array-size 1)
                          (row-pitch 0)
                          (slice-pitch 0)
                          data
                          buffer)
  (let* ((flags (join-flags flags))
         (num-mip-levels 0)
         (num-samples 0))
    (with-foreign-object (format '(:struct cl-image-format))
      (setf (foreign-slot-value format '(:struct cl-image-format)
                                :image-channel-order)
            image-channel-order)
      (setf (foreign-slot-value format '(:struct cl-image-format)
                                :image-channel-data-type)

            image-channel-data-type)
      (with-foreign-object (desc '(:struct cl-image-desc))
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-type)
              image-type)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-width)
              width)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-height)
              height)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-depth)
              depth)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-array-size)
              array-size)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-row-pitch)
              row-pitch)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :image-slice-pitch)
              slice-pitch)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :num-mip-levels)
              num-mip-levels)
        (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                  :num-samples)
              num-samples)
        (when buffer
          (setf (foreign-slot-value desc '(:struct cl-image-desc)
                                    :buffer)
                buffer))
        (if data
            (let* ((ndata (length data))
                   (data-type (image-channel-type->data-type
                               image-channel-data-type)))
              (with-foreign-object (fdata data-type ndata)
                (loop
                   for i below ndata
                   for d in data
                   do (setf (mem-aref fdata data-type i)
                            d))
                (check-opencl-error err ()
                  (clCreateImage context flags format desc
                                 fdata err))))
            (check-opencl-error err ()
              (clCreateImage context flags format desc
                             +NULL+ err)))))))

(defun cl-create-pipe (context flags packet-size max-packets)
  (let* ((properties +NULL+)) ; as of OpenCL 2.0
    (check-opencl-error err ()
      (clCreatePipe context
                    (join-flags flags)
                    packet-size
                    max-packets
                    properties
                    err))))

(defun cl-release-mem-object (obj)
  (check-opencl-error () ()
    (clReleaseMemObject obj)))

(defun cl-retain-mem-object (obj)
  (check-opencl-error () ()
    (clRetainMemObject obj)))

(defun cl-get-supported-image-formats (context flags
                                       &key (image-type
                                             +CL-MEM-OBJECT-IMAGE2D+))
  "Returns list of plists for the supported image formats."
  (let* ((flags (join-flags flags)))
    (with-foreign-object (num-formats 'cl-uint)
      (check-opencl-error () ()
        (clGetSupportedImageFormats context flags image-type
                                    0 +NULL+ num-formats))
      (with-foreign-object (formats '(:struct cl-image-format)
                                    (mem-ref num-formats 'cl-uint))
        (check-opencl-error () ()
          (clGetSupportedImageFormats context flags image-type
                                      (mem-ref num-formats 'cl-uint)
                                      formats +NULL+))
        (loop
           for i below (mem-ref num-formats 'cl-uint)
           collecting (convert-from-foreign
                       (mem-aptr formats '(:struct cl-image-format) i)
                       '(:struct cl-image-format)))))))

(defun cl-get-mem-object-info (obj param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetMemObjectInfo obj param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clGetMemObjectInfo obj param (mem-ref retsize 'size-t)
                            retval +NULL+))
      (let* ((rettype
              (case= param
                (+CL-MEM-TYPE+
                 'cl-mem-object-type)
                (+CL-MEM-FLAGS+
                 'cl-mem-flags)
                (+CL-MEM-SIZE+
                 'size-t)
                (+CL-MEM-HOST-PTR+
                 :pointer)
                (+CL-MEM-MAP-COUNT+
                 'cl-uint)
                (+CL-MEM-REFERENCE-COUNT+
                 'cl-uint)
                (+CL-MEM-CONTEXT+
                 'cl-context))))
        (mem-ref retval rettype)))))

(defun cl-get-image-info (image param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetImageInfo image param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clGetImageInfo image param (mem-ref retsize 'size-t)
                        retval +NULL+))
      (let* ((rettype
              (case= param
                (+CL-IMAGE-FORMAT+
                 '(:struct cl-image-format))
                (+CL-IMAGE-ELEMENT-SIZE+
                 'size-t)
                (+CL-IMAGE-ROW-PITCH+
                 'size-t)
                (+CL-IMAGE-SLICE-PITCH+
                 'size-t)
                (+CL-IMAGE-WIDTH+
                 'size-t)
                (+CL-IMAGE-HEIGHT+
                 'size-t)
                (+CL-IMAGE-DEPTH+
                 'size-t)
                ;; Uncomment this once Windows header is handled
                ;; (+CL-IMAGE-D3D10-SUBRESOURCE-KHR+
                ;;  :pointer)
                )))
        (mem-ref retval rettype)))))

(defun cl-get-pipe-info (pipe param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetPipeInfo pipe param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clGetPipeInfo pipe param (mem-ref retsize 'size-t)
                       retval +NULL+))
      (let* ((rettype
              (case= param
                (+CL-PIPE-PACKET-SIZE+
                 'cl-uint)
                (+CL-PIPE-MAX-PACKETS+
                 'cl-uint))))
        (mem-ref retval rettype)))))

(defun cl-set-mem-object-destructor-callback
    (obj callback
     &key
       (user-data +NULL+))
  (check-opencl-error () ()
    (clSetMemObjectDestructorCallback obj callback user-data)))

;; SVM Allocation API
(defun cl-svm-alloc (context flags size
                     &key (alignment 0))
  "Returns SVM pointer on success.  Note that there is no error status
returned by OpenCL for clSVMAlloc, so neither is there any error
management in this API."
  (clSVMAlloc context
              (join-flags flags)
              size
              alignment))

(defun cl-svm-free (context svm-pointer)
  "Frees SVM pointer on success.  Note that there is no error status
returned by OpenCL for clSVMFree, so neither is there any error
management in this API."
  (clSVMFree context svm-pointer))

;; Sampler APIs
(defun cl-create-sampler (context
                          &key
                            (sampler-normalized-coords
                             +CL-TRUE+)
                            (sampler-addressing-mode
                             +CL-ADDRESS-CLAMP+)
                            (sampler-filter-mode
                             +CL-FILTER-NEAREST+)
                            ;; extensions
                            sampler-mip-filter-mode-khr
                            sampler-lod-min-khr
                            sampler-lod-max-khr)
  "Creates sampler with given settings.  NOTE: At the time of writing,
the headers on my system did not have the CL_FILTER_NEAREST_KHR
constant defined, which is the default value for the
CL_SAMPLER_MIP_FILTER_MODE_KHR property.  It's not included in the
groveler file for this reason."
  (let* ((proplist
          ;; will be reversed later
          (reverse
           (list +CL-SAMPLER-NORMALIZED-COORDS+
                 sampler-normalized-coords
                 +CL-SAMPLER-ADDRESSING-MODE+
                 sampler-addressing-mode
                 +CL-SAMPLER-FILTER-MODE+
                 sampler-filter-mode))))
    (when sampler-mip-filter-mode-khr
      (push +CL-SAMPLER-MIP-FILTER-MODE-KHR+
            proplist)
      (push sampler-mip-filter-mode-khr
            proplist))
    (when sampler-lod-min-khr
      (push +CL-SAMPLER-LOD-MIN-KHR+
            proplist)
      (push sampler-lod-min-khr
            proplist))
    (when sampler-lod-max-khr
      (push +CL-SAMPLER-LOD-MAX-KHR+
            proplist)
      (push sampler-lod-max-khr
            proplist))
    (setf proplist (reverse proplist))
    (let* ((nprops (length proplist)))
      (with-foreign-object (props 'cl-sampler-properties
                                  nprops)
        (loop
           for p in proplist
           for i from 0
           do (setf (mem-aref props 'cl-sampler-properties i)
                    p))
        (setf (mem-aref props 'cl-sampler-properties nprops)
              0)
        (check-opencl-error err ()
          (clCreateSamplerWithProperties context
                                         props
                                         err))))))

(defun cl-retain-sampler (sampler)
  (check-opencl-error () ()
    (clRetainSampler sampler)))

(defun cl-release-sampler (sampler)
  (check-opencl-error () ()
    (clReleaseSampler sampler)))

(defun cl-get-sampler-info (sampler param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetSamplerInfo sampler param 0 +NULL+ retsize))
    (let* ((rettype
            (case= param
              (+CL-SAMPLER-REFERENCE-COUNT+
               'cl-uint)
              (+CL-SAMPLER-CONTEXT+
               'cl-context)
              (+CL-SAMPLER-ADDRESSING-MODE+
               'cl-addressing-mode)
              (+CL-SAMPLER-FILTER-MODE+
               'cl-filter-mode)
              (+CL-SAMPLER-NORMALIZED-COORDS+
               'cl-bool))))
      (with-foreign-object (retval :char (mem-ref retsize 'size-t))
        (check-opencl-error () ()
          (clGetSamplerInfo sampler param
                            (mem-ref retsize 'size-t)
                            retval
                            +NULL+))
        (mem-ref retval rettype)))))

;; Program API
(defun cl-create-program-with-source (context source)
  "Creates program using source.  source may either be a string or a
list of source strings."
  (let* ((sources (if (listp source)
                      source
                      (list source)))
         (nsources (length sources))
         (strings (loop
                     for source in sources
                     collecting
                       (foreign-string-alloc source))))
    (with-foreign-object (strings-ptr :pointer nsources)
      (loop
         for i below nsources
         for s in strings
         do (setf (mem-aref strings-ptr :pointer i)
                  s))
      (let* ((result
              (check-opencl-error err ()
                (clCreateProgramWithSource context
                                           1
                                           strings-ptr
                                           +NULL+
                                           err))))
        (loop for s in strings
           do (foreign-free s))
        result))))

;; Utility for reading binary data from file into Lisp array
(defun read-binary-data-from-pathname (pathname)
  "Reads binary data from file located at pathname and returns an
array of (unsigned-byte 8) data suitable for use with the OpenCL
high-level API binary data functions,
e.g. cl-create-program-with-binary."
  (with-open-file (file pathname
                        :direction :input
                        :element-type '(unsigned-byte 8))
    (let* ((len (file-length file))
           (result (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence result file)
      result)))

;; Utility for writing binary data from a Lisp array into a file
(defun write-binary-data-to-pathname
    (data pathname
     &key
       (if-exists nil if-exists-p)
       (if-does-not-exist nil if-does-not-exist-p))
  "Writes binary data to file located at pathname from an array
of (unsigned-byte 8) data suitable for use with the OpenCL high-level
API binary data functions."
  (let* ((file (apply #'open pathname
                      :direction :output
                      :element-type '(unsigned-byte 8)
                      (append
                       (when if-exists-p
                         (list :if-exists if-exists))
                       (when if-does-not-exist-p
                         (list :if-does-not-exist if-does-not-exist))))))
    (write-sequence data file)
    (close file)))

(defun cl-create-program-with-binary (context devices binary-arrays)
  "Creates a program given a list of devices and a list of binary data
arrays, one binary data array per supplied device.  Each array should
contain integer elements between the values 0 and 255 inclusively to
denote bytes of data.  A type of (UNSIGNED-BYTE 8) is recommended for
the array element type to optimize storage.  Data located in files can
be loaded with read-binary-data-from-pathname."
  (let* ((lengths (mapcar #'length binary-arrays))
         (ndevs (length devices)))
    (with-foreign-objects ((devs 'cl-device-id ndevs)
                           (binaries-ptr :pointer ndevs)
                           (len-ptr 'size-t ndevs))
      (loop
         for dev in devices
         for len in lengths
         for i from 0
         do
           (setf (mem-aref devs 'cl-device-id i)
                 dev)
           (setf (mem-aref len-ptr 'size-t i)
                 len))
      (let* ((binaries
              (loop
                 for len in lengths
                 collecting (foreign-alloc :uchar :count len))))
        (loop
           for i below ndevs
           for bin in binaries
           do (setf (mem-aref binaries-ptr :pointer i)
                    bin))
        (loop
           for bin in binaries
           for len in lengths
           for arr in binary-arrays
           do (loop
                 for i below len
                 do (setf (mem-aref bin :uchar i)
                          (aref arr i))))
        (let* ((retval
                (check-opencl-error err ()
                  (clCreateProgramWithBinary context
                                             ndevs
                                             devs
                                             len-ptr
                                             binaries-ptr
                                             +NULL+ ; might use this eventually
                                             err))))
          (loop
             for bin in binaries
             do (foreign-free bin))
          retval)))))

(defun cl-create-program-with-built-in-kernels
    (context devices kernels)
  "Creates program from built-in kernel names.  devices is a list of
cl-device-id device handles, and kernels is either a list of Lisp
strings or a single string of semicolon-separated names denoting the
kernel names to include in the program.  Note that all kernels must be
defined in all devices for this to succeed."
  (let* ((ndevs (length devices))
         (kernstr
          (if (stringp kernels)
              kernels
              (reduce (lambda (x y)
                        (concatenate 'string
                                     x
                                     ";"
                                     y))
                      kernels))))
    (with-foreign-object (devs 'cl-device-id ndevs)
      (loop
         for d in devices
         for i from 0
         do (setf (mem-aref devs 'cl-device-id i)
                  d))
      (with-foreign-string (kernstr-ptr kernstr)
        (check-opencl-error err ()
          (clCreateProgramWithBuiltInKernels context
                                             ndevs
                                             devs
                                             kernstr
                                             err))))))

(defun cl-create-program-with-il (context il)
  "Creates program from intermediate language binary data.  il should
be an array where the elements denote bytes, so (unsigned-byte 8) is
the recommended element type. read-binary-data-from-pathname is
available to load binary data stored in a file into such an array."
  (let* ((nbytes (length il)))
    (with-foreign-object (data :uchar nbytes)
      (loop
         for i below nbytes
         for d in il
         do (setf (mem-aref data :uchar i)
                  d))
      (check-opencl-error err ()
        (clCreateProgramWithIL context
                               il
                               nbytes
                               err)))))

(defun cl-release-program (program)
  (check-opencl-error () ()
    (clReleaseProgram program)))

(defun cl-retain-program (program)
  (check-opencl-error () ()
    (clRetainProgram program)))

(defun cl-build-program (program devices
                         &key
                           notify-fn
                           options
                           user-data)
  "Builds program for devices.  notify-function must be a callback
defined with cffi:defcallback, and user-data must be a CFFI pointer to
allocated foreign data.  Note that (callback somecallback) must be
supplied as an argument as per usual CFFI usage."
  (let* ((cb (if notify-fn
                 notify-fn
                 +NULL+))
         (data (if user-data
                   user-data
                   +NULL+))
         (opts (if options
                   (foreign-string-alloc options)
                   +NULL+))
         (ndevices (length devices)))
    (with-foreign-object (devs 'CL-DEVICE-ID ndevices)
      (loop
         for i below ndevices
         for d in devices
         do (setf (mem-aref devs 'CL-DEVICE-ID i)
                  d))
      (check-opencl-error () ()
        (clBuildProgram program ndevices devs opts cb data)))))

(defun cl-compile-program (program devices
                           &key
                             options
                             header-programs
                             header-names
                             callback
                             user-data)
  "Compiles a program according to options for each of the devices
listed.  If header-programs is supplied, then header-names needs to
contain the names of each of the header-programs as referred to in the
program source code.  When supplied, user-data should be a foreign
pointer to data used by the callback function, which should be defined
using cffi:defcallback."
  (let* ((ndevs (length devices))
         (opt-ptr +NULL+)
         (nheaders 0)
         (hp-ptr +NULL+)
         (hn-ptr +NULL+)
         (cb-ptr +NULL+)
         (ud-ptr +NULL+))
    (with-foreign-object (devs 'cl-device-id ndevs)
      (loop
         for d in devices
         for i from 0
         do (setf (mem-aref devs 'cl-device-id i)
                  d))
      (when callback
        (setf cb-ptr callback))
      (when user-data
        (setf ud-ptr user-data))
      (labels ((init-headers ()
                 (when header-programs
                   (setf nheaders
                         (length header-programs))
                   (setf hp-ptr
                         (foreign-alloc 'cl-program :count nheaders))
                   (loop
                      for i below nheaders
                      for hp in header-programs
                      do (setf (mem-aref hp-ptr 'cl-program i)
                               hp))
                   (setf hn-ptr
                         (foreign-alloc :pointer :count nheaders))
                   (loop
                      for i below nheaders
                      for hn in header-names
                      do (setf (mem-aref hn-ptr :pointer i)
                               (foreign-string-alloc hn)))))
               (clean-headers ()
                 (when header-programs
                   (foreign-free hp-ptr)
                   (loop
                      for i below nheaders
                      do (foreign-free (mem-aref hn-ptr :pointer i)))
                   (foreign-free hn-ptr)))
               (init-options ()
                 (when options
                   (setf opt-ptr
                         (foreign-string-alloc options))))
               (clean-options ()
                 (when options
                   (foreign-free opt-ptr)))
               (init ()
                 (init-options)
                 (init-headers))
               (cleanup ()
                 (clean-headers)
                 (clean-options)))
        (init)
        (check-opencl-error () ()
          (clCompileProgram program ndevs devs
                            opt-ptr
                            nheaders
                            hp-ptr
                            hn-ptr
                            cb-ptr
                            ud-ptr))
        (cleanup)))))

(defun cl-link-program (context programs devices
                        &key
                          options
                          callback
                          user-data)
  "Links programs into executables for devices.  programs must be a
list of programs to include in the executable.  callback can be a
cffi:defcallback return value, and user-data can be a foreign pointer
to data supplied to that callback."
  (let* ((ndevs (length devices))
         (ninputs 0)
         (opt-ptr +NULL+)
         (ip-ptr +NULL+)
         (cb-ptr +NULL+)
         (ud-ptr +NULL+))
    (with-foreign-object (devs 'cl-device-id ndevs)
      (when callback
        (setf cb-ptr callback))
      (when user-data
        (setf ud-ptr user-data))
      (labels ((init ()
                 (setf ninputs (length programs))
                 (setf ip-ptr (foreign-alloc 'cl-program :count ninputs))
                 (loop
                    for ip in programs
                    for i from 0
                    do (setf (mem-aref ip-ptr 'cl-program i)
                             ip))
                 (when options
                   (setf opt-ptr (foreign-string-alloc options))))
               (cleanup ()
                 (foreign-free ip-ptr)
                 (when options
                   (foreign-free opt-ptr))))
        (init)
        (check-opencl-error err ()
          (clLinkProgram context ndevs devs
                         opt-ptr
                         ninputs
                         ip-ptr
                         cb-ptr
                         ud-ptr
                         err))
        (cleanup)))))

(defun cl-set-program-release-callback (program callback
                                        &key
                                          (user-data +NULL+))
  "Sets release callback for program.  callback must be a return value
of cffi:defcallback.  user-data can be a pointer to foreign data given
to the callback."
  (check-opencl-error () ()
    (clSetProgramReleaseCallback program callback user-data)))

;; Note: cl-set-program-specialization-constant would be defined, but
;; I can't find sufficient documentation to create a Lisp version.
;; You can still call this from the CFFI lower level API as
;; clSetProgramSpecializationConstant.

(defun cl-unload-platform-compiler (platform)
  (check-opencl-error () ()
    (clUnloadPlatformCompiler platform)))

(defun cl-get-program-info (program param)
  "Get program info from parameters.  NOTE: +CL-PROGRAM-BINARIES+
triggers an error message referring to the helper function needed for
extracting binaries."
  (when (= param +CL-PROGRAM-BINARIES+)
    (error "Use cl-get-program-binaries to extract binaries from a program."))
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetProgramInfo program param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clGetProgramInfo program param (mem-ref retsize 'size-t)
                          retval
                          +NULL+))
      (case= param
        (+CL-PROGRAM-REFERENCE-COUNT+
         (mem-ref retval 'cl-uint))
        (+CL-PROGRAM-CONTEXT+
         (mem-ref retval 'cl-context))
        (+CL-PROGRAM-NUM-DEVICES+
         (mem-ref retval 'cl-uint))
        (+CL-PROGRAM-DEVICES+
         (let* ((ndevices (floor (mem-aref retsize 'size-t)
                                 (foreign-type-size 'cl-device-id))))
           (loop
              for i below ndevices
              collecting (mem-aref retval 'cl-device-id i))))
        (+CL-PROGRAM-SOURCE+
         (foreign-string-to-lisp retval))
        (+CL-PROGRAM-BINARY-SIZES+
         (let* ((nbinaries (floor (mem-aref retsize 'size-t)
                                  (foreign-type-size 'size-t))))
           (loop
              for i below nbinaries
              collecting (mem-aref retval 'size-t i))))
        (+CL-PROGRAM-NUM-KERNELS+
         (mem-ref retval 'size-t))
        (+CL-PROGRAM-KERNEL-NAMES+
         (foreign-string-to-lisp retval))))))

(defun cl-get-program-binaries (program)
  "Convenience function returning list of binary data arrays for the
binaries of a program.  Wrapper around necessary calls to
cl-get-program-info and foreign memory management."
  (let* ((binary-sizes
          (cl-get-program-info program +CL-PROGRAM-BINARY-SIZES+))
         (nbinaries (length binary-sizes))
         (binary-buffers
          (loop
             for size in binary-sizes
             collecting (foreign-alloc :uchar :count size))))
    (with-foreign-object (binbuf-ptr :pointer nbinaries)
      (loop
         for buf in binary-buffers
         for i from 0
         do (setf (mem-aref binbuf-ptr :pointer i)
                  buf))
      (with-foreign-object (retsize 'size-t)
        (check-opencl-error () ()
          (clGetProgramInfo program +CL-PROGRAM-BINARIES+
                            0 +NULL+ retsize))
        (check-opencl-error () ()
          (clGetProgramInfo program +CL-PROGRAM-BINARIES+
                            (mem-ref retsize 'size-t)
                            binbuf-ptr
                            +NULL+))
        (loop
           for i below nbinaries
           for size in binary-sizes
           collecting (foreign-array-to-lisp
                       (mem-aref binbuf-ptr :pointer i)
                       (list :array :uchar size)
                       :element-type '(unsigned-byte 8)))))))

(defun cl-get-program-build-info (program device param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetProgramBuildInfo program device param
                             0 +NULL+ retsize))
    (with-foreign-object (retval :uchar (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clGetProgramBuildInfo program device param
                               (mem-ref retsize 'size-t)
                               retval +NULL+))
      (case= param
        (+CL-PROGRAM-BUILD-STATUS+
         (mem-ref retval 'cl-build-status))
        (+CL-PROGRAM-BUILD-OPTIONS+
         (foreign-string-to-lisp retval))
        (+CL-PROGRAM-BUILD-LOG+
         (foreign-string-to-lisp retval))))))

;; Kernel API
(defun cl-create-kernel (program name)
  (check-opencl-error err ()
    (with-foreign-string (sname name)
      (clCreateKernel program sname err))))

(defun cl-create-kernels-in-program (program)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clCreateKernelsInProgram program 0 +NULL+ retsize))
    (with-foreign-object (kernels 'cl-kernel (mem-ref retsize 'size-t))
      (check-opencl-error () ()
        (clCreateKernelsInProgram program (mem-ref retsize 'size-t)
                                  kernels +NULL+))
      (loop
         for i below (mem-ref retsize 'size-t)
         collecting (mem-aref kernels 'cl-kernel i)))))

(defun cl-clone-kernel (kernel)
  (check-opencl-error err ()
    (clCloneKernel kernel err)))

(defun cl-retain-kernel (kernel)
  (check-opencl-error () ()
    (clRetainKernel kernel)))

(defun cl-release-kernel (kernel)
  (check-opencl-error () ()
    (clReleaseKernel kernel)))

(defun cl-set-kernel-arg (kernel arg-index
                          &key
                            value
                            (type 'cl-mem)
                            (count 1)
                            size)
  "Sets kernel argument.  If value is NIL, local memory will be
supplied to the kernel.  Otherwise, the value will be assumed to be of
foreign type.  Size is determined by the type and count unless size is
explicitly specified.  Note that count only has meaning when value is
not supplied and therefore only matters when allocating local memory
as a kernel argument."
  (when (not (or value size))
    (error "Must set value or size in cl-set-kernel-arg"))
  (let* ((size (if size
                   size
                   (* count (foreign-type-size type)))))
    (if value
        (with-foreign-object (f type)
          (setf (mem-ref f type)
                value)
          (check-opencl-error () ()
            (clSetKernelArg kernel arg-index size f)))
        (check-opencl-error () ()
          (clSetKernelArg kernel arg-index size +NULL+)))))

(defun cl-set-kernel-arg-svm-pointer (kernel arg-index value)
  (check-opencl-error () ()
    (clSetKernelArgSVMPointer kernel arg-index value)))

(defun cl-set-kernel-exec-info (kernel
                                &key
                                  svm-pointers
                                  fine-grain-system)
  "Adjusts kernel execution settings.  svm-pointers can be a list of
SVM foreign pointer objects.  fine-grain-system can be either
+CL-TRUE+ or +CL-FALSE+, but the default depends on the system.  Both
arguments can be set to adjust both settings simultaneously."
  (when (not (or svm-pointers fine-grain-system))
    (error "Must set either svm-pointers or fine-grain-system."))
  (when svm-pointers
    (let* ((np (length svm-pointers)))
      (with-foreign-object (p :pointer np)
        (loop
           for sp in svm-pointers
           for i from 0
           do (setf (mem-aref p :pointer i)
                    sp))
        (check-opencl-error () ()
          (clSetKernelExecInfo kernel +CL-KERNEL-EXEC-INFO-SVM-PTRS+
                               (* (foreign-type-size :pointer)
                                  np)
                               p)))))
  (when fine-grain-system
    (with-foreign-object (fgs 'cl-bool)
      (setf (mem-aref fgs 'cl-bool)
            fine-grain-system)
      (check-opencl-error () ()
        (clSetKernelExecInfo kernel +CL-KERNEL-EXEC-INFO-SVM-FINE-GRAIN-SYSTEM+
                             (foreign-type-size 'cl-bool)
                             fgs)))))

(defun cl-get-kernel-info (kernel param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetKernelInfo kernel param
                       0 +NULL+
                       retsize))
    (let ((size (mem-ref retsize 'size-t)))
      (with-foreign-object (retval :uchar size)
        (check-opencl-error () ()
          (clGetKernelInfo kernel param
                           size retval
                           +NULL+))
        (case= param
          (+CL-KERNEL-FUNCTION-NAME+
           (foreign-string-to-lisp retval))
          (+CL-KERNEL-NUM-ARGS+
           (mem-ref retval 'cl-uint))
          (+CL-KERNEL-REFERENCE-COUNT+
           'cl-uint)
          (+CL-KERNEL-CONTEXT+
           'cl-context)
          (+CL-KERNEL-PROGRAM+
           'cl-program)
          (+CL-KERNEL-ATTRIBUTES+
           (foreign-string-to-lisp retval)))))))

(defun cl-get-kernel-arg-info (kernel arg-index param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error () ()
      (clGetKernelArgInfo kernel arg-index
                          param
                          0 +NULL+
                          retsize))
    (let ((size (mem-ref retsize 'size-t)))
      (with-foreign-object (retval :uchar size)
        (check-opencl-error () ()
          (clGetKernelArgInfo kernel arg-index
                              param
                              size retval
                              +NULL+))
        (case= param
          (+CL-KERNEL-ARG-ADDRESS-QUALIFIER+
           (mem-ref retval 'cl-kernel-arg-address-qualifier))
          (+CL-KERNEL-ARG-ACCESS-QUALIFIER+
           (mem-ref retval 'cl-kernel-arg-access-qualifier))
          (+CL-KERNEL-ARG-TYPE-NAME+
           (foreign-string-to-lisp retval))
          (+CL-KERNEL-ARG-TYPE-QUALIFIER+
           (mem-ref retval 'cl-kernel-arg-type-qualifier))
          (+CL-KERNEL-ARG-NAME+
           (foreign-string-to-lisp retval)))))))

(defun cl-get-kernel-work-group-info (kernel device param)
  "Queries kernel for information about running on given device."
  (let* ((type nil)
         (count 1))
    (case= param
      (+CL-KERNEL-WORK-GROUP-SIZE+
       (setf type 'size-t))
      (+CL-KERNEL-COMPILE-WORK-GROUP-SIZE+
       (setf type 'size-t)
       (setf count 3))
      (+CL-KERNEL-LOCAL-MEM-SIZE+
       (setf type 'cl-ulong)))
    (with-foreign-object (result type count)
      (check-opencl-error () ()
        (clGetKernelWorkGroupInfo kernel device param
                                  (* count (foreign-type-size type))
                                  result
                                  +NULL+))
      (case= param
        (+CL-KERNEL-WORK-GROUP-SIZE+
         (mem-ref result 'size-t))
        (+CL-KERNEL-COMPILE-WORK-GROUP-SIZE+
         (loop
            for i below 3
            collecting (mem-aref result 'size-t i)))
        (+CL-KERNEL-LOCAL-MEM-SIZE+
         (mem-ref result 'cl-uint))))))

(defun cl-get-kernel-sub-group-info (kernel device param
                                     &key input)
  "Queries kernel sub-group information.  input must be set to
reasonable values per param value.

+CL-KERNEL-MAX-SUB-GROUP-SIZE-FOR-NDRANGE+: list of integers.
+CL-KERNEL-SUB-GROUP-COUNT-FOR-NDRANGE+: list of integers.
+CL-KERNEL-LOCAL-SIZE-FOR-SUB-GROUP-COUNT+: integer.
+CL-KERNEL-MAX-NUM-SUB-GROUPS+: ignored.
+CL-KERNEL-COMPILE-NUM-SUB-GROUPS+: ignored."
  (case= param
    (+CL-KERNEL-MAX-SUB-GROUP-SIZE-FOR-NDRANGE+
     (let* ((ninput (length input)))
       (with-foreign-object (ptr 'size-t ninput)
         (loop
            for in in input
            for i from 0
            do (setf (mem-ref ptr 'size-t)
                     in))
         (with-foreign-object (retsize 'size-t)
           (check-opencl-error () ()
             (clGetKernelSubGroupInfo kernel device param
                                      (* (foreign-type-size 'size-t)
                                         ninput)
                                      ptr
                                      0 +NULL+
                                      retsize))
           (with-foreign-object (retval :uchar (mem-ref retsize 'size-t))
             (check-opencl-error () ()
               (clGetKernelSubGroupInfo kernel device param
                                        (* (foreign-type-size 'size-t)
                                           ninput)
                                        ptr
                                        (mem-ref retsize 'size-t)
                                        retval +NULL+))
             (mem-ref retval 'size-t))))))
    (+CL-KERNEL-SUB-GROUP-COUNT-FOR-NDRANGE+
     (let* ((ninput (length input)))
       (with-foreign-object (ptr 'size-t ninput)
         (loop
            for in in input
            for i from 0
            do (setf (mem-ref ptr 'size-t)
                     in))
         (with-foreign-object (retsize 'size-t)
           (check-opencl-error () ()
             (clGetKernelSubGroupInfo kernel device param
                                      (* (foreign-type-size 'size-t)
                                         ninput)
                                      ptr
                                      0 +NULL+
                                      retsize))
           (with-foreign-object (retval :uchar (mem-ref retsize 'size-t))
             (check-opencl-error () ()
               (clGetKernelSubGroupInfo kernel device param
                                        (* (foreign-type-size 'size-t)
                                           ninput)
                                        ptr
                                        (mem-ref retsize 'size-t)
                                        retval +NULL+))
             (mem-ref retval 'size-t))))))
    (+CL-KERNEL-LOCAL-SIZE-FOR-SUB-GROUP-COUNT+
     (with-foreign-objects ((ptr 'size-t)
                            (retsize 'size-t))
       (setf (mem-ref ptr 'size-t) input)
       (check-opencl-error () ()
         (clGetKernelSubGroupInfo kernel device param
                                  (foreign-type-size 'size-t)
                                  ptr
                                  0
                                  +NULL+
                                  retsize))
       (let* ((nret (floor (mem-ref retsize 'size-t)
                           (foreign-type-size 'size-t))))
         (with-foreign-object (retval 'size-t nret)
           (check-opencl-error () ()
             (clGetKernelSubGroupInfo kernel device param
                                      (foreign-type-size 'size-t)
                                      ptr
                                      (mem-ref retsize 'size-t)
                                      retval
                                      +NULL+))
           (foreign-array-to-lisp retval (list :array 'size-t nret))))))
    (+CL-KERNEL-MAX-NUM-SUB-GROUPS+
     (with-foreign-object (retsize 'size-t)
       (check-opencl-error () ()
         (clGetKernelSubGroupInfo kernel device param
                                  0
                                  +NULL+
                                  0
                                  +NULL+
                                  retsize))
       (with-foreign-object (retval 'size-t)
         (check-opencl-error () ()
           (clGetKernelSubGroupInfo kernel device param
                                    0
                                    +NULL+
                                    (mem-ref retsize 'size-t)
                                    retval
                                    +NULL+))
         (mem-ref retval 'size-t))))
    (+CL-KERNEL-COMPILE-NUM-SUB-GROUPS+
     (with-foreign-object (retsize 'size-t)
       (check-opencl-error () ()
         (clGetKernelSubGroupInfo kernel device param
                                  0
                                  +NULL+
                                  0
                                  +NULL+
                                  retsize))
       (with-foreign-object (retval 'size-t)
         (check-opencl-error () ()
           (clGetKernelSubGroupInfo kernel device param
                                    0
                                    +NULL+
                                    (mem-ref retsize 'size-t)
                                    retval
                                    +NULL+))
         (mem-ref retval 'size-t))))))

;; Event Object APIs
(defun cl-wait-for-events (events)
  "Waits for events and returns input event list for possible
release."
  (let* ((n (length events)))
    (with-foreign-object (evs 'cl-event n)
      (loop
         for i below n
         for ev in events
         do (setf (mem-ref evs 'cl-event i)
                  ev))
      (check-opencl-error () ()
        (clWaitForEvents n
                         evs))
      events)))

(defun cl-wait-and-release-events (events)
  "Waits for and releases events once they have completed."
  (mapcar #'cl-release-event
          (cl-wait-for-events events)))

(defun cl-get-event-info (event param)
  (let* ((type
          (cond
            ((= param +CL-EVENT-COMMAND-QUEUE+)
             'cl-command-queue)
            ((= param +CL-EVENT-CONTEXT+)
             'cl-context)
            ((= param +CL-EVENT-COMMAND-TYPE+)
             'cl-command-type)
            ((= param +CL-EVENT-COMMAND-EXECUTION-STATUS+)
             'cl-int)
            ((= param +CL-EVENT-REFERENCE-COUNT+)
             'cl-uint))))
    (with-foreign-object (retsize 'size-t)
      (check-opencl-error () ()
        (clGetEventInfo event param
                        0 +NULL+
                        retsize))
      (with-foreign-object (retval type)
        (check-opencl-error () ()
          (clGetEventInfo event param
                          (mem-ref retsize 'size-t)
                          retval
                          +NULL+))))))

(defun cl-create-user-event (context)
  (check-opencl-error err ()
    (clCreateUserEvent context err)))

(defun cl-retain-event (event)
  (check-opencl-error () ()
    (clRetainEvent event)))

(defun cl-release-event (event)
  (check-opencl-error () ()
    (clReleaseEvent event)))

(defun cl-set-user-event-status (event status)
  (check-opencl-error () ()
    (clSetUserEventStatus event status)))

(defun cl-set-event-callback (event callback-type callback
                              &key
                                user-data)
  (check-opencl-error () ()
    (clSetEventCallback event callback-type callback
                        (if user-data
                            user-data
                            +NULL+))))

;; Profiling APIs
(defun cl-get-event-profiling-info (event param)
  (let* ((type
          (case= param
            (+CL-PROFILING-COMMAND-QUEUED+
             'cl-ulong)
            (+CL-PROFILING-COMMAND-SUBMIT+
             'cl-ulong)
            (+CL-PROFILING-COMMAND-START+
             'cl-ulong)
            (+CL-PROFILING-COMMAND-END+
             'cl-ulong)
            (+CL-PROFILING-COMMAND-COMPLETE+
             'cl-ulong))))
    (with-foreign-object (retsize 'size-t)
      (check-opencl-error () ()
        (clGetEventProfilingInfo event param
                                 0 +NULL+
                                 retsize))
      (with-foreign-object (retval type)
        (check-opencl-error () ()
          (clGetEventProfilingInfo event param
                                   (mem-ref retsize 'size-t)
                                   retval
                                   +NULL+))))))

;; Flush and Finish APIs
(defun cl-flush (queue)
  (check-opencl-error () ()
    (clFlush queue)))

(defun cl-finish (queue)
  (check-opencl-error () ()
    (clFinish queue)))

;; Enqueued Commands APIs
(defun cl-enqueue-read-buffer (queue buffer array-type
                               &key
                                 (offset 0)
                                 make-array-args
                                 event-wait-list
                                 blocking-p)
  "Enqueues reading from a buffer in the command queue.  array-type
should be a CFFI array type denoting how the foreign array data is
stored, i.e. (:array element-type dim1-size dim2-size ...).
make-array-args will be passed to cffi:foreign-array-to-lisp along
with the read data and the array-type.  There are 2 main modes of
operation:

blocking-p NIL: Asynchronous read.  cl-enqueue-read-buffer will
return a list with two elements: An event handle and a function that
when called with no arguments will return the data read out of the
buffer as well as cleaning up allocated foreign memory.  If an error
occurs while attempting to enqueue the read instruction, the foreign
memory will be freed before signaling an error.  The event handle
returned will need to eventually have cl-release-event called on it to
avoid a memory leak.

blocking-p non-NIL: Synchronous read.  cl-enqueue-read-buffer will
return the data read out of the buffer.

offset can denote an offset in bytes at which to start reading from
the buffer.

NOTE: Do not throw away the buffer read function for asynchronous
mode, as there is no reasonable way to avoid a memory leak.  Even with
finalization, the OpenCL queue would need to be blocked by the
finalizer to avoid a segfault."
  (destructuring-bind (type &rest dims) (rest array-type)
    (let* ((size (apply #'*
                        (foreign-type-size type)
                        dims))
           (ptr (foreign-alloc type :count size))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-array-alloc (coerce event-wait-list 'array)
                                                 (list :array 'cl-event
                                                       (length event-wait-list)))))
                      (loop
                         for x in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  x))
                      res)
                    +NULL+)))
      (labels ((cleanupptr ()
                 (foreign-free ptr))
               (cleanupewl ()
                 (when event-wait-list
                   (foreign-free ewl)))
               (cleanup ()
                 (cleanupptr)
                 (cleanupewl)))
        (with-foreign-object (event 'cl-event)
          (if blocking-p
              (progn
                (check-opencl-error () #'cleanup
                  (clEnqueueReadBuffer queue
                                       buffer
                                       +CL-TRUE+
                                       offset
                                       size
                                       ptr
                                       (if event-wait-list
                                           (length event-wait-list)
                                           0)
                                       ewl
                                       ;; no need to use event handle
                                       +NULL+))
                (let* ((result (apply #'foreign-array-to-lisp
                                      ptr
                                      array-type
                                      make-array-args)))
                  (cleanup)
                  result))
              (progn
                (check-opencl-error () #'cleanup
                  (clEnqueueReadBuffer queue
                                       buffer
                                       +CL-FALSE+
                                       offset
                                       size
                                       ptr
                                       (if event-wait-list
                                           (length event-wait-list)
                                           0)
                                       ewl
                                       event))
                (cleanupewl)
                (list (mem-ref event 'cl-event)
                      (lambda ()
                        (let* ((result (apply #'foreign-array-to-lisp
                                              ptr
                                              array-type
                                              make-array-args)))
                          (cleanupptr)
                          result))))))))))

(defun cl-enqueue-read-buffer-rect
    (queue buffer array-type
     &key
       (width 1)
       (height 1)
       (depth 1)
       buffer-origin
       (buffer-row-pitch 0)
       (buffer-slice-pitch 0)
       make-array-args
       event-wait-list
       blocking-p)
  "Enqueues rectangular reading from a buffer in the command queue.
array-type should be a CFFI array type denoting how the foreign array
data is stored, i.e. (:array element-type dim1-size dim2-size ...).
make-array-args will be passed to cffi:foreign-array-to-lisp along
with the read data and the array-type.  There are 2 main modes of
operation:

blocking-p NIL: Asynchronous read.  cl-enqueue-read-buffer will
return a list with two elements: An event handle and a function that
when called with no arguments will return the data read out of the
buffer as well as cleaning up allocated foreign memory.  If an error
occurs while attempting to enqueue the read instruction, the foreign
memory will be freed before signaling an error.  The event handle
returned will need to eventually have cl-release-event called on it to
avoid a memory leak.

blocking-p non-NIL: Synchronous read.  cl-enqueue-read-buffer will
return the data read out of the buffer.

buffer-origin can be NIL or a list of 3 elements denoting the 3-D
origin to start readinfrom the buffer.  Set unused dimensions' origins
to 0.

width, height, and depth need to be set to reasonable values.  Note
that width denotes the number of elements rather than bytes.  You can
make them match your array-type, but all that is required is that the
array-type has enough space to store the data.

NOTE: Do not throw away the buffer read function for asynchronous
mode, as there is no reasonable way to avoid a memory leak.  Even with
finalization, the OpenCL queue would need to be blocked by the
finalizer to avoid a segfault."
  (destructuring-bind (type &rest dims) (rest array-type)
    (let* ((element-size (foreign-type-size type))
           (size (apply #'*
                        (foreign-type-size type)
                        dims))
           (ptr (foreign-alloc type :count size))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-array-alloc (coerce event-wait-list 'array)
                                                 (list :array 'cl-event
                                                       (length event-wait-list)))))
                      (loop
                         for x in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  x))
                      res)
                    +NULL+)))
      (with-foreign-objects ((region 'size-t 3)
                             (buffer-origin-ptr 'size-t 3)
                             (host-origin-ptr 'size-t 3))
        (loop
           for i below 2
           do (setf (mem-aref host-origin-ptr 'size-t i)
                    0))
        (if buffer-origin
            (loop
               for x in buffer-origin
               for i from 0
               do (setf (mem-aref buffer-origin-ptr 'size-t i)
                        x))
            (loop
               for i below 2
               do (setf (mem-aref buffer-origin-ptr 'size-t i)
                        0)))
        (setf (mem-aref region 'size-t 0)
              (* element-size width))
        (setf (mem-aref region 'size-t 1)
              height)
        (setf (mem-aref region 'size-t 2)
              depth)
        (labels ((cleanupptr ()
                   (foreign-free ptr))
                 (cleanupewl ()
                   (when event-wait-list
                     (foreign-free ewl)))
                 (cleanup ()
                   (cleanupptr)
                   (cleanupewl)))
          (with-foreign-object (event 'cl-event)
            (if blocking-p
                (progn
                  (check-opencl-error () #'cleanup
                    (clEnqueueReadBufferRect queue
                                             buffer
                                             +CL-TRUE+
                                             buffer-origin-ptr
                                             host-origin-ptr
                                             region
                                             buffer-row-pitch
                                             buffer-slice-pitch
                                             0
                                             0
                                             ptr
                                             (if event-wait-list
                                                 (length event-wait-list)
                                                 0)
                                             ewl
                                             ;; no need to use event handle
                                             +NULL+))
                  (let* ((result (apply #'foreign-array-to-lisp
                                        ptr
                                        array-type
                                        make-array-args)))
                    (cleanup)
                    result))
                (progn
                  (check-opencl-error () #'cleanup
                    (clEnqueueReadBufferRect queue
                                             buffer
                                             +CL-FALSE+
                                             buffer-origin-ptr
                                             host-origin-ptr
                                             region
                                             buffer-row-pitch
                                             buffer-slice-pitch
                                             0
                                             0
                                             ptr
                                             (if event-wait-list
                                                 (length event-wait-list)
                                                 0)
                                             ewl
                                             event))
                  (cleanupewl)
                  (list (mem-ref event 'cl-event)
                        (lambda ()
                          (let* ((result (apply #'foreign-array-to-lisp
                                                ptr
                                                array-type
                                                make-array-args)))
                            (cleanupptr)
                            result)))))))))))

(defun cl-enqueue-write-buffer (queue buffer element-type data
                                &key
                                  blocking-p
                                  (offset 0)
                                  event-wait-list)
  "Enqueues write instructions for the buffer and supplied data.
  There are two modes of operation: blocking and non-blocking.

blocking-p is NIL: Asynchronous operation.  Data is queued for
writing, but not necessarily written on return.  The return value is a
list containing an event handle and a cleanup function to free the
foreign data once the write has completed.

blocking-p is non-NIL: Synchronous operation.  The return value
is irrelevant and no foreign memory management is necessary.

element-type should be a CFFI type.

offset is a byte-offset into the output buffer.

event-wait-list can be a list of events required to finish before the
write should occur."
  (let* ((n (length data))
         (size (* n (foreign-type-size element-type)))
         (ptr (foreign-alloc element-type :count n))
         (ewl
          (if event-wait-list
              (let* ((res
                      (foreign-array-alloc (coerce event-wait-list 'array)
                                           (list :array 'cl-event
                                                 (length event-wait-list)))))
                (loop
                   for x in event-wait-list
                   for i from 0
                   do (setf (mem-aref res 'cl-event i)
                            x))
                res)
              +NULL+)))
    (loop
       for d in data
       for i from 0
       do (setf (mem-aref ptr element-type i)
                d))
    (labels ((cleanupptr ()
               (foreign-free ptr))
             (cleanupewl ()
               (when event-wait-list
                 (foreign-free ewl)))
             (cleanup ()
               (cleanupptr)
               (cleanupewl)))
      (with-foreign-object (event 'cl-event)
        (if blocking-p
            (progn
              (check-opencl-error () #'cleanup
                (clEnqueueWriteBuffer queue
                                      buffer
                                      +CL-TRUE+
                                      offset
                                      size
                                      ptr
                                      (if event-wait-list
                                          (length event-wait-list)
                                          0)
                                      ewl
                                      +NULL+))
              (cleanup)
              NIL)
            (progn
              (check-opencl-error () #'cleanup
                (clEnqueueWriteBuffer queue
                                      buffer
                                      +CL-FALSE+
                                      offset
                                      size
                                      ptr
                                      (if event-wait-list
                                          (length event-wait-list)
                                          0)
                                      ewl
                                      event))
              (cleanupewl)
              (list (mem-ref event 'cl-event)
                    #'cleanupptr)))))))

(defun sequence->3D-array (sequence width height depth
                           &key
                             (index-mode :first)
                             make-array-key-args)
  "Converts sequence into 3-D array of width-height-depth dimensions.
make-array-key-args will be supplied to make-array.  The ordering
taken from sequence is controlled via the index-mode option.

index-mode :first means (0 0 0), (1 0 0), ..., (0 1 0), (1 1 0), ... index ordering.
index-mode :last means (0 0 0), (0 0 1), ..., (0 1 0), (0 1 1), ... index ordering.

So for example:
(sequence->3d-array (list 1 2 3 4 5 6) 3 2 1 :index-mode :first) ==> #3A(((1) (4)) ((2) (5)) ((3) (6))),
(sequence->3d-array (list 1 2 3 4 5 6) 3 2 1 :index-mode :last) ==> #3A(((1) (2)) ((3) (4)) ((5) (6)))"
  (let* ((result (apply #'make-array
                        (list width height depth)
                        make-array-key-args))
         (index 0))
    (labels ((indexlast (i)
               (list (floor i (* depth height))
                     (mod (floor i depth) height)
                     (mod i depth))
               )
             (indexfirst (i)
               (list (mod i width)
                     (mod (floor i width) height)
                     (floor i (* width height)))))
      (let* ((ifn (if (eq index-mode :first)
                      (lambda (x)
                        (setf (apply #'aref result (indexfirst (1- (incf index))))
                              x))
                      (lambda (x)
                        (setf (apply #'aref result (indexlast (1- (incf index))))
                              x)))))
        (map nil ifn sequence)
        result))))

(defun 3D-array->list (array
                       &key
                         (index-mode :first))
  "Converts a 3-D array into a list.  The ordering taken from sequence
is controlled via the index-mode option.

index-mode :first means (0 0 0), (1 0 0), ..., (0 1 0), (1 1 0), ... index ordering.
index-mode :last means (0 0 0), (0 0 1), ..., (0 1 0), (0 1 1), ... index ordering.

So for example:
(3D-array->list #3A(((1) (4)) ((2) (5)) ((3) (6))) :index-mode :first) ==> (1 2 3 4 5 6),
(3D-array->list #3A(((1) (4)) ((2) (5)) ((3) (6))) :index-mode :last) ==> (1 4 2 5 3 6)."
  (destructuring-bind (width height depth)
      (array-dimensions array)
    (labels ((indexlast (i)
               (list (floor i (* depth height))
                     (mod (floor i depth) height)
                     (mod i depth))
               )
             (indexfirst (i)
               (list (mod i width)
                     (mod (floor i width) height)
                     (floor i (* width height)))))
      (let* ((size (* width height depth)))
        (if (eq index-mode :first)
            (loop
               for i below size
               collecting (apply #'aref array (indexfirst i)))
            (loop
               for i below size
               collecting (apply #'aref array (indexlast i))))))))

(defun cl-enqueue-write-buffer-rect (queue buffer element-type data
                                     &key
                                       (width 1)
                                       (height 1)
                                       (depth 1)
                                       buffer-origin
                                       (buffer-row-pitch 0)
                                       (buffer-slice-pitch 0)
                                       event-wait-list
                                       blocking-p)
  "Enqueues rectangular write into buffer.  There are two modes of operation:

blocking-p NIL: Asynchronous write.  Return value is a list
containing an event handle and a cleanup function to call once the
event has completed in order to free allocated foreign memory.

data should be a list or a 3-D array.  If data is a 3-D array, then
width-height-depth will be set to match the array.  If data is a list,
then the width-height-depth parameters should match the number of
elements in the list.  The 3D-array->list utility function will be
used to convert a 3-D array into a list if a 3-D array is supplied.
To control index ordering, call 3D-array->list directly.  Default
behavior of 3D-array->sequence is :index-mode :first, see function
documentation.  OpenCL uses :first index convention.

blocking-p non-NIL: Synchronous write.  Return value is NIL.

buffer-origin can be a list of integers to denote an offset in the
buffer write destination (see OpenCL documentation), or NIL to denote
the default non-offset write.

Set width, height, and depth to reasonable values if using a data
list.  Note that width refers to the number of elements rather than
bytes, which is computed from element-type."
  (let* ((data (if (typep data 'sequence)
                   (sequence->3D-array data width height depth)
                   data))
         (datalist (if (listp data)
                       data
                       (3D-array->list data)))
         (width (if (listp data)
                    width
                    (first (array-dimensions data))))
         (height (if (listp data)
                     height
                     (second (array-dimensions data))))
         (depth (if (listp data)
                    data
                    (third (array-dimensions data))))
         (ptr (let* ((res
                      (foreign-alloc element-type :count (* width height depth))))
                (loop
                   for d in datalist
                   for i from 0
                   do (setf (mem-aref res element-type i)
                            d))
                res))
         (ewl (if event-wait-list
                  (let ((res (foreign-alloc 'cl-event :count
                                            (length event-wait-list))))
                    (loop
                       for ev in event-wait-list
                       for i from 0
                       do (setf (mem-aref res 'cl-event i)
                                ev))
                    res)
                  +NULL+))
         (buffer-origin-ptr
          (let* ((res (foreign-alloc 'size-t :count 3)))
            (if buffer-origin
                (loop
                   for x in buffer-origin
                   for i from 0
                   do (setf (mem-aref res 'size-t i)
                            x))
                (loop
                   for i below 3
                   do (setf (mem-aref res 'size-t i)
                            0)))
            res))
         (host-origin-ptr
          (let* ((res (foreign-alloc 'size-t :count 3)))
            (loop
               for i below 3
               do (setf (mem-aref res 'size-t i) 0))
            res)))
    (labels ((cleanuptmp ()
               (foreign-free host-origin-ptr)
               (foreign-free buffer-origin-ptr)
               (when event-wait-list
                 (foreign-free ewl)))
             (cleanupdata ()
               (foreign-free ptr))
             (cleanup ()
               (cleanuptmp)
               (cleanupdata)))
      (with-foreign-objects ((region 'size-t 3))
        (setf (mem-aref region 'size-t 0)
              (* width (foreign-type-size element-type)))
        (setf (mem-aref region 'size-t 1)
              height)
        (setf (mem-aref region 'size-t 2)
              depth)
        (if blocking-p
            (progn
              (check-opencl-error () #'cleanup
                (clEnqueueWriteBufferRect queue
                                          buffer
                                          +CL-TRUE+
                                          buffer-origin-ptr
                                          host-origin-ptr
                                          region
                                          buffer-row-pitch
                                          buffer-slice-pitch
                                          0
                                          0
                                          ptr
                                          (if event-wait-list
                                              (length event-wait-list)
                                              0)
                                          ewl
                                          +NULL+))
              (cleanup)
              NIL)
            (with-foreign-object (event 'cl-event)
              (check-opencl-error () #'cleanup
                (clEnqueueWriteBufferRect queue
                                          buffer
                                          +CL-TRUE+
                                          buffer-origin-ptr
                                          host-origin-ptr
                                          region
                                          buffer-row-pitch
                                          buffer-slice-pitch
                                          0
                                          0
                                          ptr
                                          (if event-wait-list
                                              (length event-wait-list)
                                              0)
                                          ewl
                                          event))
              (cleanuptmp)
              (list (mem-ref event 'cl-event)
                    #'cleanupdata)))))))

(defun cl-enqueue-fill-buffer (queue buffer byte-pattern size
                               &key
                                 (offset 0)
                                 event-wait-list)
  "Enqueues fill write to buffer.  byte-pattern should be a list of
(unsigned-byte 8) values that will repeatedly be written into the
buffer.  size should be the number of bytes in total to write.

There is only asynchronous operation for this write function. Return
value is a list of an event and a cleanup function to call once the
event has completed."
  (let* ((n (length byte-pattern))
         (ptr (foreign-alloc :uchar :count n))
         (ewl
          (if event-wait-list
              (let* ((res
                      (foreign-array-alloc (coerce event-wait-list 'array)
                                           (list :array 'cl-event
                                                 (length event-wait-list)))))
                (loop
                   for x in event-wait-list
                   for i from 0
                   do (setf (mem-aref res 'cl-event i)
                            x))
                res)
              +NULL+)))
    (loop
       for d in byte-pattern
       for i from 0
       do (setf (mem-aref ptr :uchar i)
                d))
    (labels ((cleanupptr ()
               (foreign-free ptr))
             (cleanupewl ()
               (when event-wait-list
                 (foreign-free ewl)))
             (cleanup ()
               (cleanupptr)
               (cleanupewl)))
      (with-foreign-object (event 'cl-event)
        (check-opencl-error () #'cleanup
          (clEnqueueFillBuffer queue
                               buffer
                               ptr
                               n
                               offset
                               size
                               (if event-wait-list
                                   (length event-wait-list)
                                   0)
                               ewl
                               event))
        (cleanupewl)
        (list (mem-ref event 'cl-event)
              #'cleanupptr)))))

(defun cl-enqueue-copy-buffer (queue src-buffer dst-buffer size
                               &key
                                 (src-offset 0)
                                 (dst-offset 0)
                                 event-wait-list)
  "Enqueues copy from src-buffer to dst-buffer.  Return value is a an
event for copy completion."
  (let* ((ewl (if event-wait-list
                  (let* ((res
                          (foreign-alloc 'cl-event :count (length event-wait-list))))
                    (loop
                       for ev in event-wait-list
                       for i from 0
                       do (setf (mem-ref res 'cl-event i)
                                ev))
                    res)
                  +NULL+)))
    (labels ((cleanup ()
               (when event-wait-list
                 (foreign-free ewl))))
      (with-foreign-object (event 'cl-event)
        (check-opencl-error () #'cleanup
          (clEnqueueCopyBuffer queue
                               src-buffer
                               dst-buffer
                               src-offset
                               dst-offset
                               size
                               (if event-wait-list
                                   (length event-wait-list)
                                   0)
                               ewl
                               event))
        (cleanup)
        (mem-ref event 'cl-event)))))

(defun cl-enqueue-copy-buffer-rect
    (queue src-buffer dst-buffer
     &key
       (width 1)
       (height 1)
       (depth 1)
       (src-row-pitch 0)
       (src-slice-pitch 0)
       src-origin
       (dst-row-pitch 0)
       (dst-slice-pitch 0)
       dst-origin
       event-wait-list)
  "Enqueues rectangular copy from src-buffer to dst-buffer.  Return
value is an event for copy completion.

src-origin and dst-origin can be lists with 3 elements denoting the
origin in the source and destination buffers respectively.

NOTE: In contrast to other similar functions, width should denote the
width in bytes, while height and depth are the number of rows and
slices respectively.  The product (* width height depth) will be the
total amount of data copied."
  (let* ((ewl (if event-wait-list
                  (let* ((res
                          (foreign-alloc 'cl-event :count (length event-wait-list))))
                    (loop
                       for ev in event-wait-list
                       for i from 0
                       do (setf (mem-ref res 'cl-event i)
                                ev))
                    res)
                  +NULL+))
         (src-origin-ptr (let* ((res
                                 (foreign-alloc 'size-t :count 3)))
                           (if src-origin
                               (loop
                                  for x in src-origin
                                  for i below 3
                                  do (setf (mem-aref res 'size-t i)
                                           x))
                               (loop
                                  for i below 3
                                  do (setf (mem-aref res 'size-t i)
                                           0)))
                           res))
         (dst-origin-ptr (let* ((res
                                 (foreign-alloc 'size-t :count 3)))
                           (if dst-origin
                               (loop
                                  for x in dst-origin
                                  for i below 3
                                  do (setf (mem-aref res 'size-t i)
                                           x))
                               (loop
                                  for i below 3
                                  do (setf (mem-aref res 'size-t i)
                                           0)))
                           res)))
    (with-foreign-object (region 'size-t 3)
      (setf (mem-aref region 'size-t 0)
            width)
      (setf (mem-aref region 'size-t 1)
            height)
      (setf (mem-aref region 'size-t 2)
            depth)
      (labels ((cleanup ()
                 (foreign-free src-origin-ptr)
                 (foreign-free dst-origin-ptr)
                 (when event-wait-list
                   (foreign-free ewl))))
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueCopyBufferRect queue
                                     src-buffer
                                     dst-buffer
                                     src-origin-ptr
                                     dst-origin-ptr
                                     region
                                     src-row-pitch
                                     src-slice-pitch
                                     dst-row-pitch
                                     dst-slice-pitch
                                     (if event-wait-list
                                         (length event-wait-list)
                                         0)
                                     ewl
                                     event))
          (cleanup)
          (mem-ref event 'cl-event))))))

;; Parse pixel into Lisp
(defun format->type-info (format)
  (destructuring-bind (&key image-channel-data-type
                            image-channel-order)
      format
    (let* ((ndims
            (case= image-channel-order
              (+CL-R+ 1)
              (+CL-RX+ 1)
              (+CL-A+ 1)
              (+CL-INTENSITY+ 1)
              (+CL-LUMINANCE+ 1)
              (+CL-DEPTH+ 1)
              (+CL-RG+ 2)
              (+CL-RGX+ 2)
              (+CL-RA+ 2)
              (+CL-RGB+ 3)
              (+CL-RGBX+ 3)
              (+CL-RGBA+ 4)
              (+CL-SRGB+ 3)
              (+CL-SRGBX+ 3)
              (+CL-SRGBA+ 4)
              (+CL-SBGRA+ 4)
              (+CL-ARGB+ 4)
              (+CL-BGRA+ 4)
              (+CL-ABGR+ 4)
              (+CL-DEPTH-STENCIL+ 1)))
           (type (image-channel-type->data-type image-channel-data-type)))
      (list ndims type))))

(defun pixel->lisp (pixel-ptr format)
  "Takes foreign data located in pixel-ptr with plist format and
returns Lisp data suitable for that pixel type.  For 1-channel pixels,
numerical value is returned.  For multi-channel pixels, a list of
numerical values is returned."
  (destructuring-bind (ndims type)
      (format->type-info format)
    (if (= ndims 1)
        (mem-ref pixel-ptr type)
        (loop
           for i below ndims
           collecting (mem-aref pixel-ptr type i)))))

;; Lisp to pixel
(defun lisp->pixel! (lisp-pixel ptr format)
  "Places pixel data in foreign ptr formatted by plist format and
using Lisp data lisp-pixel suitable for that pixel type.  1-channel
pixels use numeric types, and multi-channel pixels use a list of
numerical values."
  (destructuring-bind (ndims type) (format->type-info format)
    (if (= ndims 1)
        (setf (mem-ref ptr type)
              lisp-pixel)
        (loop
           for x in lisp-pixel
           for i below ndims
           do (setf (mem-aref ptr type i)
                    x)))))

;; Byte array to pixel
(defun bytes->pixel! (byte-array ptr format
                      &key (start 0))
  "Places pixel data from an array of (unsigned-byte 8) data at start
in foreign ptr formatted by plist format and using Lisp data
lisp-pixel suitable for that pixel type.  1-channel pixels use numeric
types, and multi-channel pixels use a list of numerical values."
  (destructuring-bind (ndims type) (format->type-info format)
    (let* ((size (* (foreign-type-size type) ndims)))
      (loop
         for xi from start
         for i below size
         do (setf (mem-aref ptr :uchar i)
                  (aref byte-array xi))))))

(defun pixel->bytes! (ptr format byte-array
                      &key
                        (start 0))
  "Converts pixel data at ptr into (unsigned-byte 8) data and places
it in byte-array starting at start."
  (destructuring-bind (ndims type)
      (format->type-info format)
    (let* ((size (* ndims (foreign-type-size type))))
      (loop
         for i below size
         for xi from start
         do (setf (aref byte-array xi)
                  (mem-aref ptr :uchar i))))))

;; Make the image read-write functions accept either Lisp-formatted
;; data or unsigned-byte arrays for binary I/O.

(defun cl-enqueue-read-image (queue image
                              &key
                                bytes-p
                                blocking-p
                                origin
                                region
                                (row-pitch 0)
                                (slice-pitch 0)
                                event-wait-list)
  "Enqueues a read command into the image buffer.  Image data result
 is either a 3-D array of pixel->lisp formatted data matching channel
 dimensions of the image, or if bytes-p is non-NIL, an array
 of (unsigned-byte 8) data matching the size of the image.

There are two modes of operation: Synchronous and asynchronous.

blocking-p non-NIL: Synchronous operation.  Result is returned
directly.  blocking-p NIL: Asynchronous operation.  Result is a
list of an event and a function to call which will return the
previously mentioned array of formatted data.

origin can be a list of three pixel indices (x y z) denoting the
starting point for reading from the image.  Default value is (0 0 0).

region can be a list of (width height depth) in pixels denoting the
region of pixels to read starting at origin.  Default region is the
entire image."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((image-format
            (cl-get-image-info image +CL-IMAGE-FORMAT+))
           (element-size
            (cl-get-image-info image +CL-IMAGE-ELEMENT-SIZE+))
           (origin (if origin
                       origin
                       (list 0 0 0)))
           (origin-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for i below 3
                            for x in origin
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (total-size (* element-size height width depth))
           (ptr (foreign-alloc :uchar :count total-size))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanuptmp ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free origin-ptr)
                 (foreign-free region-ptr))
               (cleanupdata ()
                 (foreign-free ptr))
               (cleanup ()
                 (cleanuptmp)
                 (cleanupdata))
               (readbytes ()
                 (let* ((arr (make-array total-size
                                         :element-type '(unsigned-byte 8))))
                   (loop
                      for i below total-size
                      do (setf (aref arr i)
                               (mem-aref ptr :uchar i)))
                   arr))
               (readlisp ()
                 (let* ((arr (make-array (list width height depth))))
                   (loop
                      for i below (first region)
                      do
                        (loop
                           for j below (second region)
                           do
                             (loop
                                for k below depth
                                do
                                  (let* ((index (+ i
                                                   (* width j)
                                                   (* width height k))))
                                    (setf (aref arr i j k)
                                          (pixel->lisp
                                           (mem-aptr ptr :uchar (* index
                                                                   element-size))
                                           image-format))))))
                   arr)))
        (let* ((readptr (if bytes-p
                            #'readbytes
                            #'readlisp)))
          (if blocking-p
              (progn
                (check-opencl-error () #'cleanup
                  (clEnqueueReadImage queue
                                      image
                                      +CL-TRUE+
                                      origin-ptr
                                      region-ptr
                                      row-pitch
                                      slice-pitch
                                      ptr
                                      (if event-wait-list
                                          (length event-wait-list)
                                          0)
                                      ewl
                                      +NULL+))
                (let* ((result
                        (funcall readptr)))
                  (cleanup)
                  result))
              (with-foreign-object (event 'cl-event)
                (check-opencl-error () #'cleanup
                  (clEnqueueReadImage queue
                                      image
                                      +CL-FALSE+
                                      origin-ptr
                                      region-ptr
                                      row-pitch
                                      slice-pitch
                                      ptr
                                      (if event-wait-list
                                          (length event-wait-list)
                                          0)
                                      ewl
                                      event))
                (cleanuptmp)
                (list (mem-ref event 'cl-event)
                      (lambda ()
                        (let* ((result (funcall readptr)))
                          (cleanupdata)
                          result))))))))))

(defun cl-enqueue-write-image (queue image data
                               &key
                                 bytes-p
                                 blocking-p
                                 origin
                                 region
                                 (row-pitch 0)
                                 (slice-pitch 0)
                                 event-wait-list)
  "Enqueues a write command into the image buffer.  Image data input
must either be 3-D array of pixel->lisp formatted data matching the
channel dimensions of the image, or if bytes-p is non-NIL, an array
of (unsigned-byte 8) data matching the size of the image.

There are two modes of operation: Synchronous and asynchronous.

blocking-p non-NIL: Synchronous operation.  Result is NIL.

blocking-p NIL: Asynchronous operation.  Result is a list of an event
and a function to call which will cleanup foreign memory.

origin can be a list of three pixel indices (x y z) denoting the
starting point for reading from the image.  Default value is (0 0 0).

region can be a list of (width height depth) in pixels denoting the
region of pixels to read starting at origin.  Default region is the
entire image."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((image-format
            (cl-get-image-info image +CL-IMAGE-FORMAT+))
           (element-size
            (cl-get-image-info image +CL-IMAGE-ELEMENT-SIZE+))
           (origin (if origin
                       origin
                       (list 0 0 0)))
           (origin-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for i below 3
                            for x in origin
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (total-size (* element-size height width depth))
           (ptr (foreign-alloc :uchar :count total-size))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanuptmp ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free origin-ptr)
                 (foreign-free region-ptr))
               (cleanupdata ()
                 (foreign-free ptr))
               (cleanup ()
                 (cleanuptmp)
                 (cleanupdata))
               (writebytes ()
                 (loop
                    for i below total-size
                    do (setf (mem-aref ptr :uchar i)
                             (aref data i))))
               (writelisp ()
                 (loop
                    for i below (first region)
                    do
                      (loop
                         for j below (second region)
                         do
                           (loop
                              for k below depth
                              do
                                (let* ((index (+ i
                                                 (* width j)
                                                 (* width height k))))
                                  (lisp->pixel! (aref data i j k)
                                                (mem-aptr ptr :uchar (* index
                                                                        element-size))
                                                image-format)))))))
        (if bytes-p
            (writebytes)
            (writelisp))
        (if blocking-p
            (progn
              (check-opencl-error () #'cleanup
                (clEnqueueWriteImage queue
                                     image
                                     +CL-TRUE+
                                     origin-ptr
                                     region-ptr
                                     row-pitch
                                     slice-pitch
                                     ptr
                                     (if event-wait-list
                                         (length event-wait-list)
                                         0)
                                     ewl
                                     +NULL+))
              (cleanup)
              nil)
            (with-foreign-object (event 'cl-event)
              (check-opencl-error () #'cleanup
                (clEnqueueWriteImage queue
                                     image
                                     +CL-FALSE+
                                     origin-ptr
                                     region-ptr
                                     row-pitch
                                     slice-pitch
                                     ptr
                                     (if event-wait-list
                                         (length event-wait-list)
                                         0)
                                     ewl
                                     event))
              (cleanuptmp)
              (list (mem-ref event 'cl-event)
                    #'cleanupdata)))))))

(defun fill-image-channel-type->data-type (channel-data-type)
  (case= channel-data-type
    (+CL-SNORM-INT8+
     :float)
    (+CL-SNORM-INT16+
     :float)
    (+CL-UNORM-INT8+
     :float)
    (+CL-UNORM-INT16+
     :float)
    (+CL-UNORM-SHORT-565+
     :float)
    (+CL-UNORM-SHORT-555+
     :float)
    (+CL-UNORM-INT-101010+
     :float)
    (+CL-SIGNED-INT8+
     :int)
    (+CL-SIGNED-INT16+
     :int)
    (+CL-SIGNED-INT32+
     :int)
    (+CL-UNSIGNED-INT8+
     :uint)
    (+CL-UNSIGNED-INT16+
     :uint)
    (+CL-UNSIGNED-INT32+
     :uint)
    (+CL-HALF-FLOAT+
     'cl-half)
    (+CL-FLOAT+
     :float)))

(defun cl-enqueue-fill-image (queue image pixel-data
                              &key
                                origin
                                region
                                event-wait-list)
  "Enqueues a fill command into the image buffer.  Pixel data input
must be either a single floating-point number or a 4-element list of
either floats or integers depending on the channel type.  See
clEnqueueFillImage OpenCL documentation for rules on when to use
integers or floats.

There is only one mode: Asynchronous operation.  Result is a list of
an event and a function to call which will cleanup foreign memory.

origin can be a list of three pixel indices (x y z) denoting the
starting point for reading from the image.  Default value is (0 0 0).

region can be a list of (width height depth) in pixels denoting the
region of pixels to read starting at origin.  Default region is the
entire image."
  (labels ((minone (x)
             (if (zerop x)
                 1
                 x))
           (format->type-info (format)
             (destructuring-bind (&key image-channel-data-type
                                       image-channel-order)
                 format
               (let* ((ndims
                       (case= image-channel-order
                         (+CL-R+ 1)
                         (+CL-RX+ 1)
                         (+CL-A+ 1)
                         (+CL-INTENSITY+ 1)
                         (+CL-LUMINANCE+ 1)
                         (+CL-DEPTH+ 1)
                         (+CL-RG+ 2)
                         (+CL-RGX+ 2)
                         (+CL-RA+ 2)
                         (+CL-RGB+ 3)
                         (+CL-RGBX+ 3)
                         (+CL-RGBA+ 4)
                         (+CL-SRGB+ 3)
                         (+CL-SRGBX+ 3)
                         (+CL-SRGBA+ 4)
                         (+CL-SBGRA+ 4)
                         (+CL-ARGB+ 4)
                         (+CL-BGRA+ 4)
                         (+CL-ABGR+ 4)
                         (+CL-DEPTH-STENCIL+ 1)))
                      (type (fill-image-channel-type->data-type image-channel-data-type)))
                 (list ndims type))))
           (lisp->pixel! (lisp-pixel ptr format)
             ;; Places pixel data in foreign ptr formatted by plist
             ;; format and using fill-image-channel-type->data-type to
             ;; convert lisp-pixel.  lisp-pixel is either a floating
             ;; point number for +CL-DEPTH+, a list of 4 floats for
             ;; normalized integer channels, or a list of 4 signed or
             ;; unsigned integers for unnormalized integer channels.
             (destructuring-bind (ndims type) (format->type-info format)
               (if (= ndims 1)
                   (setf (mem-ref ptr type)
                         lisp-pixel)
                   (loop
                      for x in lisp-pixel
                      for i below ndims
                      do (setf (mem-aref ptr type i)
                               x))))))
    (let* ((image-format
            (cl-get-image-info image +CL-IMAGE-FORMAT+))
           (origin (if origin
                       origin
                       (list 0 0 0)))
           (origin-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for i below 3
                            for x in origin
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           ;; technically sometimes too big
           (ptr (foreign-alloc :uchar :count (* 4 (foreign-type-size :float))))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanuptmp ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free origin-ptr)
                 (foreign-free region-ptr))
               (cleanupdata ()
                 (foreign-free ptr))
               (cleanup ()
                 (cleanuptmp)
                 (cleanupdata))
               (writelisp ()
                 (lisp->pixel! pixel-data
                               ptr
                               image-format)))
        (writelisp)
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueFillImage queue
                                image
                                ptr
                                origin-ptr
                                region-ptr
                                (if event-wait-list
                                    (length event-wait-list)
                                    0)
                                ewl
                                event))
          (cleanuptmp)
          (list (mem-ref event 'cl-event)
                #'cleanupdata))))))

(defun cl-enqueue-copy-image (queue src-image dst-image
                              &key
                                src-origin
                                dst-origin
                                region
                                event-wait-list)
  "Enqueues an image copy command.  Asynchronous operation only.
Return value is event for copy completion.

region can be a list (width height depth).  If not supplied, default
is entire source image.

src-origin and dst-origin are source and destination origins to be
read or write respectively.

event-wait-list can be a list of events which the copy command is
dependent on."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((src-origin (if src-origin
                           src-origin
                           (list 0 0 0)))
           (src-origin-ptr (let* ((res
                                   (foreign-alloc 'size-t :count 3)))
                             (loop
                                for i below 3
                                for x in src-origin
                                do (setf (mem-aref res 'size-t i)
                                         x))
                             res))
           (dst-origin (if dst-origin
                           dst-origin
                           (list 0 0 0)))
           (dst-origin-ptr (let* ((res
                                   (foreign-alloc 'size-t :count 3)))
                             (loop
                                for i below 3
                                for x in dst-origin
                                do (setf (mem-aref res 'size-t i)
                                         x))
                             res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info src-image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info src-image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info src-image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanup ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free src-origin-ptr)
                 (foreign-free dst-origin-ptr)
                 (foreign-free region-ptr)))
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueCopyImage queue
                                src-image
                                dst-image
                                src-origin-ptr
                                dst-origin-ptr
                                region-ptr
                                (if event-wait-list
                                    (length event-wait-list)
                                    0)
                                ewl
                                event))
          (cleanup)
          (mem-ref event 'cl-event))))))

(defun cl-enqueue-copy-image-to-buffer (queue src-image dst-buffer
                                        &key
                                          src-origin
                                          (dst-offset 0)
                                          region
                                          event-wait-list)
  "Enqueues an image copy to buffer command.  Asynchronous operation only.
Return value is event for copy completion.

region can be a list (width height depth).  If not supplied, default
is entire source image.

src-origin is the origin for the image read.

dst-offset is the offset for the buffer write.

event-wait-list can be a list of events which the copy command is
dependent on."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((src-origin (if src-origin
                           src-origin
                           (list 0 0 0)))
           (src-origin-ptr (let* ((res
                                   (foreign-alloc 'size-t :count 3)))
                             (loop
                                for i below 3
                                for x in src-origin
                                do (setf (mem-aref res 'size-t i)
                                         x))
                             res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info src-image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info src-image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info src-image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanup ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free src-origin-ptr)
                 (foreign-free region-ptr)))
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueCopyImageToBuffer queue
                                        src-image
                                        dst-buffer
                                        src-origin-ptr
                                        region-ptr
                                        dst-offset
                                        (if event-wait-list
                                            (length event-wait-list)
                                            0)
                                        ewl
                                        event))
          (cleanup)
          (mem-ref event 'cl-event))))))

(defun cl-enqueue-copy-buffer-to-image (queue src-buffer dst-image
                                        &key
                                          (src-offset 0)
                                          dst-origin
                                          region
                                          event-wait-list)
  "Enqueues a buffer copy to image command.  Asynchronous operation only.
Return value is event for copy completion.

region can be a list (width height depth).  If not supplied, default
is entire destination image.

src-offset is the offset for the buffer read.

dst-origin is the origin for the image write.

event-wait-list can be a list of events which the copy command is
dependent on."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((dst-origin (if dst-origin
                           dst-origin
                           (list 0 0 0)))
           (dst-origin-ptr (let* ((res
                                   (foreign-alloc 'size-t :count 3)))
                             (loop
                                for i below 3
                                for x in dst-origin
                                do (setf (mem-aref res 'size-t i)
                                         x))
                             res))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info dst-image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info dst-image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info dst-image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanup ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free dst-origin-ptr)
                 (foreign-free region-ptr)))
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueCopyBufferToImage queue
                                        src-buffer
                                        dst-image
                                        src-offset
                                        dst-origin-ptr
                                        region-ptr
                                        (if event-wait-list
                                            (length event-wait-list)
                                            0)
                                        ewl
                                        event))
          (cleanup)
          (mem-ref event 'cl-event))))))

(defun cl-enqueue-map-buffer (queue buffer flags size
                              &key
                                blocking-p
                                (offset 0)
                                event-wait-list)
  (let* ((ewl (if event-wait-list
                  (let* ((res
                          (foreign-alloc 'cl-event
                                         :count (length event-wait-list))))
                    (loop
                       for ev in event-wait-list
                       for i from 0
                       do (setf (mem-aref res 'cl-event i)
                                ev))
                    res)
                  +NULL+)))
    (labels ((cleanup ()
               (when event-wait-list
                 (foreign-free ewl))))
      (with-foreign-object (event 'cl-event)
        (if blocking-p
            (let* ((result
                    (check-opencl-error err #'cleanup
                      (clEnqueueMapBuffer queue
                                          buffer
                                          +CL-TRUE+
                                          (join-flags flags)
                                          offset
                                          size
                                          (if event-wait-list
                                              (length event-wait-list)
                                              0)
                                          ewl
                                          event
                                          err))))
              (cleanup)
              result)
            (let* ((result
                    (check-opencl-error err #'cleanup
                      (clEnqueueMapBuffer queue
                                          buffer
                                          +CL-FALSE+
                                          (join-flags flags)
                                          offset
                                          size
                                          (if event-wait-list
                                              (length event-wait-list)
                                              0)
                                          ewl
                                          event
                                          err))))
              (cleanup)
              (list (mem-ref event 'cl-event)
                    result)))))))

(defun cl-enqueue-map-image (queue image flags
                             &key
                               blocking-p
                               origin
                               region
                               event-wait-list)
  "Enqueues an image map to buffer command.  Synchronous or
asynchronous operation controlled by blocking-p.  Return value is:

Synchronous: List (pointer row-pitch slice-pitch)

Asynchronous: List (event pointer row-pitch slice-pitch) where event
is for map operation completion.

region can be a list (width height depth).  If not supplied, default
is entire source image.

origin is the origin for the image.

event-wait-list can be a list of events which the copy command is
dependent on."
  (flet ((minone (x)
           (if (zerop x)
               1
               x)))
    (let* ((origin (if origin
                       origin
                       (list 0 0 0)))
           (origin-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for i below 3
                            for x in origin
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (row-pitch-ptr (foreign-alloc 'size-t))
           (slice-pitch-ptr (foreign-alloc 'size-t))
           (width (minone
                   (if region
                       (first region)
                       (cl-get-image-info image +cl-image-width+))))
           (height (minone
                    (if region
                        (second region)
                        (cl-get-image-info image +cl-image-height+))))
           (depth (minone
                   (if region
                       (third region)
                       (cl-get-image-info image +cl-image-depth+))))
           (region (list width height depth))
           (region-ptr (let* ((res
                               (foreign-alloc 'size-t :count 3)))
                         (loop
                            for x in region
                            for i from 0
                            do (setf (mem-aref res 'size-t i)
                                     x))
                         res))
           (ewl (if event-wait-list
                    (let* ((res
                            (foreign-alloc 'cl-event
                                           :count (length event-wait-list))))
                      (loop
                         for ev in event-wait-list
                         for i from 0
                         do (setf (mem-aref res 'cl-event i)
                                  ev))
                      res)
                    +NULL+)))
      (labels ((cleanup ()
                 (when event-wait-list
                   (foreign-free ewl))
                 (foreign-free row-pitch-ptr)
                 (foreign-free slice-pitch-ptr)
                 (foreign-free origin-ptr)
                 (foreign-free region-ptr)))
        (if blocking-p
            (let* ((result
                    (check-opencl-error err #'cleanup
                      (clEnqueueMapImage queue
                                         image
                                         +CL-FALSE+
                                         (join-flags flags)
                                         origin-ptr
                                         region-ptr
                                         row-pitch-ptr
                                         slice-pitch-ptr
                                         (if event-wait-list
                                             (length event-wait-list)
                                             0)
                                         ewl
                                         +NULL+
                                         err)))
                   (row-pitch (mem-ref row-pitch-ptr 'size-t))
                   (slice-pitch (mem-ref slice-pitch-ptr 'size-t)))
              (cleanup)
              (list result row-pitch slice-pitch))
            (with-foreign-object (event 'cl-event)
              (let* ((result
                      (check-opencl-error err #'cleanup
                        (clEnqueueMapImage queue
                                           image
                                           +CL-FALSE+
                                           (join-flags flags)
                                           origin-ptr
                                           region-ptr
                                           row-pitch-ptr
                                           slice-pitch-ptr
                                           (if event-wait-list
                                               (length event-wait-list)
                                               0)
                                           ewl
                                           event
                                           err)))
                     (row-pitch (mem-ref row-pitch-ptr 'size-t))
                     (slice-pitch (mem-ref slice-pitch-ptr 'size-t)))
                (cleanup)
                (list (mem-ref event 'cl-event)
                      result
                      row-pitch
                      slice-pitch))))))))

(defun cl-enqueue-unmap-mem-object (queue obj map-ptr
                                    &key
                                      event-wait-list)
  "Enqueues unmapping obj which was mapped to map-ptr.  Asynchronous
operation only.  Return value is an event for operation completion."
  (let* ((ewl (if event-wait-list
                  (foreign-alloc 'cl-event :count (length event-wait-list))
                  +NULL+)))
    (labels ((cleanup ()
               (when event-wait-list
                 (foreign-free ewl))))
      (with-foreign-object (event 'cl-event)
        (check-opencl-error () #'cleanup
          (clEnqueueUnmapMemObject queue
                                   obj
                                   map-ptr
                                   (if event-wait-list
                                       (length event-wait-list)
                                       0)
                                   ewl
                                   event))
        (cleanup)
        (mem-ref event 'cl-event)))))

(defun cl-enqueue-migrate-mem-objects (queue objects flags
                                       &key
                                         event-wait-list)
  "Enqueues migration of memory objects to the device associated with
the supplied command queue.  Return value is an event for command
completion."
  (let* ((ewl (if event-wait-list
                  (foreign-alloc 'cl-event :count (length event-wait-list))
                  +NULL+)))
    (with-foreign-object (objects-ptr 'cl-mem (length objects))
      (loop
         for x in objects
         for i from 0
         do (setf (mem-aref objects-ptr 'cl-mem i)
                  x))
      (labels ((cleanup ()
                 (when event-wait-list
                   (foreign-free ewl))))
        (with-foreign-object (event 'cl-event)
          (check-opencl-error () #'cleanup
            (clEnqueueMigrateMemObjects queue
                                        (length objects)
                                        objects-ptr
                                        (join-flags flags)
                                        (if event-wait-list
                                            (length event-wait-list)
                                            0)
                                        ewl
                                        event))
          (cleanup)
          (mem-ref event 'cl-event))))))

(defun cl-enqueue-ndrange-kernel (queue kernel
                                  global-work-size
                                  local-work-size
                                  &key
                                    global-work-offset
                                    event-wait-list)
  "Enqueues a data-parallel kernel for execution.  global-work-size
and local-work-size must be lists that share dimensionality, and if
supplied, so must global-work-offset.

global-work-offset can denote a non-default offset for the global ID
of each thread.

Return value is an event for kernel execution completion."
  (let* ((ndims (length global-work-size))
         (ewl (if event-wait-list
                  (foreign-alloc 'cl-event :count (length event-wait-list))
                  +NULL+))
         (global-work-size-ptr
          (let* ((res
                  (foreign-alloc 'size-t :count ndims)))
            (loop
               for x in global-work-size
               for i from 0
               do (setf (mem-aref res 'size-t i)
                        x))
            res))
         (local-work-size-ptr
          (let* ((res
                  (foreign-alloc 'size-t :count ndims)))
            (loop
               for x in local-work-size
               for i from 0
               do (setf (mem-aref res 'size-t i)
                        x))
            res))
         (offset-ptr
          (let* ((res
                  (foreign-alloc 'size-t :count ndims)))
            (if global-work-offset
                (loop
                   for x in global-work-offset
                   for i from 0
                   do (setf (mem-aref res 'size-t i)
                            x))
                (loop
                   for i below ndims
                   do (setf (mem-aref res 'size-t i)
                            0)))
            res)))
    (labels ((cleanup ()
               (foreign-free offset-ptr)
               (foreign-free global-work-size-ptr)
               (foreign-free local-work-size-ptr)
               (when event-wait-list
                 (foreign-free ewl))))
      (with-foreign-object (event 'cl-event)
        (check-opencl-error () #'cleanup
          (clEnqueueNDRangeKernel queue
                                  kernel
                                  ndims
                                  offset-ptr
                                  global-work-size-ptr
                                  local-work-size-ptr
                                  (if event-wait-list
                                      (length event-wait-list)
                                      0)
                                  ewl
                                  event))
        (cleanup)
        (mem-ref event 'cl-event)))))
