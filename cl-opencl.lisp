(in-package :cl-opencl)

;; utility macro
(defmacro check-opencl-error (err &body body)
  "Macro for automating error management with two modes:

1. NULL err denoting error code will be returned by body.

2. Symbol err-address to denote symbol to use for error address so as
to interact with OpenCL CFFI functions."
  (cond
    ((null err)
     (let ((err (gensym)))
       `(let* ((,err
                (progn ,@body)))
          (when (not (= ,err +CL-SUCCESS+))
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
                (error "OpenCL Error ~a" ,err))
              ,return-value)))))))

;; Platform API
(defun cl-get-platform-ids ()
  (with-foreign-object (nplatforms :uint)
    (clgetplatformids 0 +NULL+ nplatforms)
    (let* ((nplats (mem-ref nplatforms :uint)))
      (with-foreign-object (platform-ids 'cl-platform-id nplats)
        (check-opencl-error ()
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
    (check-opencl-error ()
      (clGetPlatformInfo platform param
                         0 +NULL+
                         size))
    (with-foreign-object (pp :char (mem-ref size 'size-t))
      (check-opencl-error ()
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
        (check-opencl-error ()
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
    (check-opencl-error ()
      (clGetDeviceInfo device param
                       0 +NULL+ size))
    (with-foreign-object (p :char (mem-ref size 'size-t))
      (check-opencl-error ()
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
         (check-opencl-error ()
           (clCreateSubDevices device properties 0 +NULL+
                               num-devices-ret))
         (with-foreign-object (devices 'cl-device-id
                                       (mem-ref num-devices-ret 'cl-uint))
           (check-opencl-error ()
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
         (check-opencl-error ()
           (clCreateSubDevices device properties
                               0 +NULL+
                               num-devices-ret))
         (with-foreign-object (devices 'cl-device-id
                                       (mem-ref num-devices-ret
                                                'cl-uint))
           (check-opencl-error ()
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
           (check-opencl-error ()
             (clCreateSubDevices device properties 0 +NULL+ num-devices-ret))
           (with-foreign-object (devices 'cl-device-id
                                         (mem-ref num-devices-ret
                                                  'cl-uint))
             (check-opencl-error ()
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
  (check-opencl-error ()
    (clRetainDevice device)))

(defun cl-release-device (device)
  (check-opencl-error ()
    (clReleaseDevice device)))

(defun cl-set-default-device-command-queue (context device queue)
  (check-opencl-error ()
    (clSetDefaultDeviceCommandQueue context device queue)))

(defun cl-get-device-and-host-timer (device)
  "Returns list of (device-timestamp host-timestamp)."
  (with-foreign-objects ((devstamp 'cl-ulong)
                         (hoststamp 'cl-ulong))
    (check-opencl-error ()
      (clGetDeviceAndHostTimer device devstamp hoststamp))
    (list (mem-ref devstamp 'cl-ulong)
          (mem-ref hoststamp 'cl-ulong))))

(defun cl-get-host-timer (device)
  "Returns timestamp for host."
  (with-foreign-object (hoststamp 'cl-ulong)
    (check-opencl-error ()
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
      (check-opencl-error err
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
    (check-opencl-error err
      (clCreateContextFromType props device-type
                               (if callback
                                   callback
                                   +NULL+)
                               (if user-data
                                   user-data
                                   +NULL+)
                               err))))

(defun cl-release-context (context)
  (check-opencl-error ()
    (CLRELEASECONTEXT context)))

(defun cl-retain-context (context)
  (check-opencl-error ()
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
      (check-opencl-error err
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
          (check-opencl-error err
            (clCreateCommandQueueWithProperties context
                                                device
                                                properties
                                                err))))))

(defun cl-retain-command-queue (queue)
  (check-opencl-error ()
    (clRetainCommandQueue queue)))

(defun cl-release-command-queue (queue)
  (check-opencl-error ()
    (clReleaseCommandQueue queue)))

(defun cl-get-command-queue-info (queue param)
  (let* ((type
          (cond
            ((= param +CL-QUEUE-CONTEXT+) 'cl-context)
            ((= param +CL-QUEUE-DEVICE+) 'cl-device-id)
            ((= param +CL-QUEUE-REFERENCE-COUNT+) 'cl-uint)
            ((= param +CL-QUEUE-PROPERTIES+)
             'cl-command-queue-properties))))
    (with-foreign-object (result type)
      (check-opencl-error ()
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
        (check-opencl-error err
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
            (check-opencl-error err
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
      (check-opencl-error err
        (clCreateSubBuffer buffer flags
                           bufcreatetype
                           bufinfo
                           err)))))

(defun image-channel-type->data-type (channel-data-type)
  (cond
    ((= channel-data-type +CL-SNORM-INT8+)
     :char)
    ((= channel-data-type +CL-SNORM-INT16+)
     :short)
    ((= channel-data-type +CL-UNORM-INT8+)
     :uchar)
    ((= channel-data-type +CL-UNORM-INT16+)
     :ushort)
    ((= channel-data-type +CL-UNORM-SHORT-565+)
     :ushort)
    ((= channel-data-type +CL-UNORM-SHORT-555+)
     :ushort)
    ((= channel-data-type +CL-UNORM-INT-101010+)
     :uint)
    ((= channel-data-type +CL-SIGNED-INT8+)
     :char)
    ((= channel-data-type +CL-SIGNED-INT16+)
     :short)
    ((= channel-data-type +CL-SIGNED-INT32+)
     :int)
    ((= channel-data-type +CL-UNSIGNED-INT8+)
     :uchar)
    ((= channel-data-type +CL-UNSIGNED-INT16+)
     :ushort)
    ((= channel-data-type +CL-UNSIGNED-INT32+)
     :uint)
    ((= channel-data-type +CL-HALF-FLOAT+)
     'cl-half)
    ((= channel-data-type +CL-FLOAT+)
     :float)))

(defun cl-create-image (context flags
                        width
                        height
                        &key
                          (image-type +CL-MEM-OBJECT-IMAGE2D+)
                          (image-channel-order +CL-RGBA+)
                          (image-channel-data-type +CL-UNSIGNED-INT8+)
                          (depth 0)
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
                (check-opencl-error err
                  (clCreateImage context flags format desc
                                 fdata err))))
            (check-opencl-error err
              (clCreateImage context flags format desc
                             +NULL+ err)))))))

(defun cl-create-pipe (context flags packet-size max-packets)
  (let* ((properties +NULL+)) ; as of OpenCL 2.0
    (check-opencl-error err
      (clCreatePipe context
                    (join-flags flags)
                    packet-size
                    max-packets
                    properties
                    err))))

(defun cl-release-mem-object (obj)
  (check-opencl-error ()
    (clReleaseMemObject obj)))

(defun cl-retain-mem-object (obj)
  (check-opencl-error ()
    (clRetainMemObject obj)))

(defun cl-get-supported-image-formats (context flags
                                       &key (image-type
                                             +CL-MEM-OBJECT-IMAGE2D+))
  "Returns list of plists for the supported image formats."
  (let* ((flags (join-flags flags)))
    (with-foreign-object (num-formats 'cl-uint)
      (check-opencl-error ()
        (clGetSupportedImageFormats context flags image-type
                                    0 +NULL+ num-formats))
      (with-foreign-object (formats '(:struct cl-image-format)
                                    (mem-ref num-formats 'cl-uint))
        (check-opencl-error ()
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
    (check-opencl-error ()
      (clGetMemObjectInfo obj param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error ()
        (clGetMemObjectInfo obj param (mem-ref retsize 'size-t)
                            retval +NULL+))
      (let* ((rettype
              (cond
                ((= param +CL-MEM-TYPE+)
                 'cl-mem-object-type)
                ((= param +CL-MEM-FLAGS+)
                 'cl-mem-flags)
                ((= param +CL-MEM-SIZE+)
                 'size-t)
                ((= param +CL-MEM-HOST-PTR+)
                 :pointer)
                ((= param +CL-MEM-MAP-COUNT+)
                 'cl-uint)
                ((= param +CL-MEM-REFERENCE-COUNT+)
                 'cl-uint)
                ((= +CL-MEM-CONTEXT+)
                 'cl-context))))
        (mem-ref retval rettype)))))

(defun cl-get-image-info (image param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error ()
      (clGetImageInfo image param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error ()
        (clGetImageInfo image param (mem-ref retsize 'size-t)
                        retval +NULL+))
      (let* ((rettype
              (cond
                ((= param +CL-IMAGE-FORMAT+)
                 'cl-image-format)
                ((= param +CL-IMAGE-ELEMENT-SIZE+)
                 'size-t)
                ((= param +CL-IMAGE-ROW-PITCH+)
                 'size-t)
                ((= param +CL-IMAGE-SLICE-PITCH+)
                 'size-t)
                ((= param +CL-IMAGE-WIDTH+)
                 'size-t)
                ((= param +CL-IMAGE-HEIGHT+)
                 'size-t)
                ((= param +CL-IMAGE-DEPTH+)
                 'size-t)
                ;; Uncomment this once Windows header is handled
                ;; ((= +CL-IMAGE-D3D10-SUBRESOURCE-KHR+)
                ;;  :pointer)
                )))
        (mem-ref retval rettype)))))

(defun cl-get-pipe-info (pipe param)
  (with-foreign-object (retsize 'size-t)
    (check-opencl-error ()
      (clGetPipeInfo pipe param 0 +NULL+ retsize))
    (with-foreign-object (retval :char (mem-ref retsize 'size-t))
      (check-opencl-error ()
        (clGetPipeInfo pipe param (mem-ref retsize 'size-t)
                       retval +NULL+))
      (let* ((rettype
              (cond
                ((= param +CL-PIPE-PACKET-SIZE+)
                 'cl-uint)
                ((= param +CL-PIPE-MAX-PACKETS+)
                 'cl-uint))))
        (mem-ref retval rettype)))))

(defun cl-set-mem-object-destructor-callback
    (obj callback
     &key
       (user-data +NULL+))
  (check-opencl-error ()
    (clSetMemObjectDestructorCallback obj callback user-data)))

;; SVM Allocation API


;; Program API
(defun cl-create-program-with-source (context kernel-source)
  (check-opencl-error err
    (with-foreign-string (source kernel-source)
      (with-foreign-object (ptr :pointer)
        (setf (mem-aref ptr :pointer)
              source)
        (clCreateProgramWithSource context
                                   1
                                   ptr
                                   +NULL+
                                   err)))))

(defun cl-release-program (program)
  (check-opencl-error ()
    (clReleaseProgram program)))

(defun cl-retain-program (program)
  (check-opencl-error ()
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
      (check-opencl-error ()
        (clBuildProgram program ndevices devs opts cb data)))))

;; Kernel API
(defun cl-create-kernel (program name)
  (check-opencl-error err
    (with-foreign-string (sname name)
      (clCreateKernel program sname err))))

(defun cl-get-kernel-work-group-info (kernel device param)
  "Queries kernel for information about running on given device."
  (let* ((type nil)
         (count 1))
    (cond
      ((= param +CL-KERNEL-WORK-GROUP-SIZE+)
       (setf type 'size-t))
      ((= param +CL-KERNEL-COMPILE-WORK-GROUP-SIZE+)
       (setf type 'size-t)
       (setf count 3))
      ((= param +CL-KERNEL-LOCAL-MEM-SIZE+)
       (setf type 'cl-ulong)))
    (with-foreign-object (result type count)
      (check-opencl-error ()
        (clGetKernelWorkGroupInfo kernel device param
                                  (* count (foreign-type-size type))
                                  result
                                  +NULL+))
      (cond
        ((= param +CL-KERNEL-WORK-GROUP-SIZE+)
         (mem-ref result 'size-t))
        ((= param +CL-KERNEL-COMPILE-WORK-GROUP-SIZE+)
         (loop
            for i below 3
            collecting (mem-aref result 'size-t i)))
        ((= param +CL-KERNEL-LOCAL-MEM-SIZE+)
         (mem-ref result 'cl-uint))))))

(defun cl-set-kernel-arg (kernel index
                          &key
                            value
                            (type 'cl-mem)
                            (count 1)
                            size)
  "Sets kernel argument.  If value is NIL, local memory will be
supplied to the kernel.  Otherwise, the value will be assumed to be of
foreign type.  Size is determined by the type and count unless size is
explicitly specified."
  (let* ((size (if size
                   size
                   (* count (foreign-type-size type)))))
    (if value
        (with-foreign-object (f type)
          (setf (mem-ref f type)
                value)
          (check-opencl-error ()
            (clSetKernelArg kernel index size f)))
        (check-opencl-error ()
          (clSetKernelArg kernel index size +NULL+)))))
