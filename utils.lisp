(in-package :cl-opencl)

;;;; Utilities to make working with OpenCL easier

(defun describe-opencl-platforms ()
  "Creates a list of alists, one alist per platform, containing all
the results of cl-get-platform-ids.  The first element of each alist
is (:platform-id ID), using the same ID as returned by
cl-get-platform-ids.

Note that if a parameter causes an error, its result will not be
included in the alist."
  (let* ((syms '(+CL-PLATFORM-PROFILE+
                 +CL-PLATFORM-VERSION+
                 +CL-PLATFORM-NAME+
                 +CL-PLATFORM-VENDOR+
                 +CL-PLATFORM-EXTENSIONS+
                 +CL-PLATFORM-HOST-TIMER-RESOLUTION+
                 +CL-PLATFORM-ICD-SUFFIX-KHR+))
         (vals (list +CL-PLATFORM-PROFILE+
                     +CL-PLATFORM-VERSION+
                     +CL-PLATFORM-NAME+
                     +CL-PLATFORM-VENDOR+
                     +CL-PLATFORM-EXTENSIONS+
                     +CL-PLATFORM-HOST-TIMER-RESOLUTION+
                     +CL-PLATFORM-ICD-SUFFIX-KHR+)))
    (loop
       for pid in (cl-get-platform-ids)
       collecting
         (cons (cons :platform-id pid)
               (loop
                  for sym in syms
                  for val in vals
                  appending
                    (handler-case (list (cons sym (cl-get-platform-info pid val)))
                      (error () NIL)))))))

(defun describe-opencl-devices (platform-id device-type)
  "Creates a list of alists, one alist per device, containing all the
results of cl-get-device-ids.  The first element of each alist
is (:device-id ID), using the same ID as returned by
cl-get-device-ids.

Note that if a parameter causes an error, its result will not be
included in the alist."
  (let* ((syms '(+CL-DEVICE-ADDRESS-BITS+
                 +CL-DEVICE-AVAILABLE+
                 +CL-DEVICE-BUILT-IN-KERNELS+
                 +CL-DEVICE-COMPILER-AVAILABLE+
                 +CL-DEVICE-DOUBLE-FP-CONFIG+
                 +CL-DEVICE-ENDIAN-LITTLE+
                 +CL-DEVICE-ERROR-CORRECTION-SUPPORT+
                 +CL-DEVICE-EXECUTION-CAPABILITIES+
                 +CL-DEVICE-EXTENSIONS+
                 +CL-DEVICE-GLOBAL-MEM-CACHE-SIZE+
                 +CL-DEVICE-GLOBAL-MEM-CACHE-TYPE+
                 +CL-DEVICE-GLOBAL-MEM-CACHELINE-SIZE+
                 +CL-DEVICE-GLOBAL-MEM-SIZE+
                 +CL-DEVICE-GLOBAL-VARIABLE-PREFERRED-TOTAL-SIZE+
                 +CL-DEVICE-IL-VERSION+
                 +CL-DEVICE-IMAGE2D-MAX-HEIGHT+
                 +CL-DEVICE-IMAGE2D-MAX-WIDTH+
                 +CL-DEVICE-IMAGE3D-MAX-DEPTH+
                 +CL-DEVICE-IMAGE3D-MAX-HEIGHT+
                 +CL-DEVICE-IMAGE3D-MAX-WIDTH+
                 +CL-DEVICE-IMAGE-BASE-ADDRESS-ALIGNMENT+
                 +CL-DEVICE-IMAGE-MAX-ARRAY-SIZE+
                 +CL-DEVICE-IMAGE-MAX-BUFFER-SIZE+
                 +CL-DEVICE-IMAGE-PITCH-ALIGNMENT+
                 +CL-DEVICE-IMAGE-SUPPORT+
                 +CL-DEVICE-LINKER-AVAILABLE+
                 +CL-DEVICE-LOCAL-MEM-SIZE+
                 +CL-DEVICE-LOCAL-MEM-TYPE+
                 +CL-DEVICE-MAX-CLOCK-FREQUENCY+
                 +CL-DEVICE-MAX-COMPUTE-UNITS+
                 +CL-DEVICE-MAX-CONSTANT-ARGS+
                 +CL-DEVICE-MAX-CONSTANT-BUFFER-SIZE+
                 +CL-DEVICE-MAX-GLOBAL-VARIABLE-SIZE+
                 +CL-DEVICE-MAX-MEM-ALLOC-SIZE+
                 +CL-DEVICE-MAX-NUM-SUB-GROUPS+
                 +CL-DEVICE-MAX-ON-DEVICE-QUEUES+
                 +CL-DEVICE-MAX-PARAMETER-SIZE+
                 +CL-DEVICE-MAX-PIPE-ARGS+
                 +CL-DEVICE-MAX-READ-IMAGE-ARGS+
                 +CL-DEVICE-MAX-WRITE-IMAGE-ARGS+
                 +CL-DEVICE-MAX-READ-WRITE-IMAGE-ARGS+
                 +CL-DEVICE-MAX-SAMPLERS+
                 +CL-DEVICE-MAX-WORK-GROUP-SIZE+
                 +CL-DEVICE-MAX-WORK-ITEM-DIMENSIONS+
                 +CL-DEVICE-MAX-WORK-ITEM-SIZES+
                 +CL-DEVICE-MEM-BASE-ADDR-ALIGN+
                 +CL-DEVICE-NAME+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-CHAR+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-SHORT+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-INT+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-LONG+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-FLOAT+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-DOUBLE+
                 +CL-DEVICE-NATIVE-VECTOR-WIDTH-HALF+
                 +CL-DEVICE-OPENCL-C-VERSION+
                 +CL-DEVICE-PARENT-DEVICE+
                 +CL-DEVICE-PARTITION-AFFINITY-DOMAIN+
                 +CL-DEVICE-PARTITION-MAX-SUB-DEVICES+
                 +CL-DEVICE-PARTITION-PROPERTIES+
                 +CL-DEVICE-PARTITION-TYPE+
                 +CL-DEVICE-PIPE-MAX-ACTIVE-RESERVATIONS+
                 +CL-DEVICE-PIPE-MAX-PACKET-SIZE+
                 +CL-DEVICE-PLATFORM+
                 +CL-DEVICE-PREFERRED-GLOBAL-ATOMIC-ALIGNMENT+
                 +CL-DEVICE-PREFERRED-INTEROP-USER-SYNC+
                 +CL-DEVICE-PREFERRED-LOCAL-ATOMIC-ALIGNMENT+
                 +CL-DEVICE-PREFERRED-PLATFORM-ATOMIC-ALIGNMENT+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-CHAR+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-SHORT+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-INT+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-LONG+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-FLOAT+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-DOUBLE+
                 +CL-DEVICE-PREFERRED-VECTOR-WIDTH-HALF+
                 +CL-DEVICE-PRINTF-BUFFER-SIZE+
                 +CL-DEVICE-PROFILE+
                 +CL-DEVICE-PROFILING-TIMER-RESOLUTION+
                 +CL-DEVICE-QUEUE-ON-DEVICE-MAX-SIZE+
                 +CL-DEVICE-QUEUE-ON-DEVICE-PREFERRED-SIZE+
                 +CL-DEVICE-QUEUE-ON-DEVICE-PROPERTIES+
                 +CL-DEVICE-QUEUE-ON-HOST-PROPERTIES+
                 +CL-DEVICE-REFERENCE-COUNT+
                 +CL-DEVICE-SINGLE-FP-CONFIG+
                 +CL-DEVICE-SPIR-VERSIONS+
                 ;; +CL-DEVICE-SUBGROUP-INDEPENDENT-FORWARD-PROGRESS+
                 +CL-DEVICE-SVM-CAPABILITIES+
                 +CL-DEVICE-TERMINATE-CAPABILITY-KHR+
                 +CL-DEVICE-TYPE+
                 +CL-DEVICE-VENDOR+
                 +CL-DEVICE-VENDOR-ID+
                 +CL-DEVICE-VERSION+
                 +CL-DRIVER-VERSION+))


         (vals (list
                +CL-DEVICE-ADDRESS-BITS+
                +CL-DEVICE-AVAILABLE+
                +CL-DEVICE-BUILT-IN-KERNELS+
                +CL-DEVICE-COMPILER-AVAILABLE+
                +CL-DEVICE-DOUBLE-FP-CONFIG+
                +CL-DEVICE-ENDIAN-LITTLE+
                +CL-DEVICE-ERROR-CORRECTION-SUPPORT+
                +CL-DEVICE-EXECUTION-CAPABILITIES+
                +CL-DEVICE-EXTENSIONS+
                +CL-DEVICE-GLOBAL-MEM-CACHE-SIZE+
                +CL-DEVICE-GLOBAL-MEM-CACHE-TYPE+
                +CL-DEVICE-GLOBAL-MEM-CACHELINE-SIZE+
                +CL-DEVICE-GLOBAL-MEM-SIZE+
                +CL-DEVICE-GLOBAL-VARIABLE-PREFERRED-TOTAL-SIZE+
                +CL-DEVICE-IL-VERSION+
                +CL-DEVICE-IMAGE2D-MAX-HEIGHT+
                +CL-DEVICE-IMAGE2D-MAX-WIDTH+
                +CL-DEVICE-IMAGE3D-MAX-DEPTH+
                +CL-DEVICE-IMAGE3D-MAX-HEIGHT+
                +CL-DEVICE-IMAGE3D-MAX-WIDTH+
                +CL-DEVICE-IMAGE-BASE-ADDRESS-ALIGNMENT+
                +CL-DEVICE-IMAGE-MAX-ARRAY-SIZE+
                +CL-DEVICE-IMAGE-MAX-BUFFER-SIZE+
                +CL-DEVICE-IMAGE-PITCH-ALIGNMENT+
                +CL-DEVICE-IMAGE-SUPPORT+
                +CL-DEVICE-LINKER-AVAILABLE+
                +CL-DEVICE-LOCAL-MEM-SIZE+
                +CL-DEVICE-LOCAL-MEM-TYPE+
                +CL-DEVICE-MAX-CLOCK-FREQUENCY+
                +CL-DEVICE-MAX-COMPUTE-UNITS+
                +CL-DEVICE-MAX-CONSTANT-ARGS+
                +CL-DEVICE-MAX-CONSTANT-BUFFER-SIZE+
                +CL-DEVICE-MAX-GLOBAL-VARIABLE-SIZE+
                +CL-DEVICE-MAX-MEM-ALLOC-SIZE+
                +CL-DEVICE-MAX-NUM-SUB-GROUPS+
                +CL-DEVICE-MAX-ON-DEVICE-QUEUES+
                +CL-DEVICE-MAX-PARAMETER-SIZE+
                +CL-DEVICE-MAX-PIPE-ARGS+
                +CL-DEVICE-MAX-READ-IMAGE-ARGS+
                +CL-DEVICE-MAX-WRITE-IMAGE-ARGS+
                +CL-DEVICE-MAX-READ-WRITE-IMAGE-ARGS+
                +CL-DEVICE-MAX-SAMPLERS+
                +CL-DEVICE-MAX-WORK-GROUP-SIZE+
                +CL-DEVICE-MAX-WORK-ITEM-DIMENSIONS+
                +CL-DEVICE-MAX-WORK-ITEM-SIZES+
                +CL-DEVICE-MEM-BASE-ADDR-ALIGN+
                +CL-DEVICE-NAME+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-CHAR+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-SHORT+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-INT+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-LONG+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-FLOAT+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-DOUBLE+
                +CL-DEVICE-NATIVE-VECTOR-WIDTH-HALF+
                +CL-DEVICE-OPENCL-C-VERSION+
                +CL-DEVICE-PARENT-DEVICE+
                +CL-DEVICE-PARTITION-AFFINITY-DOMAIN+
                +CL-DEVICE-PARTITION-MAX-SUB-DEVICES+
                +CL-DEVICE-PARTITION-PROPERTIES+
                +CL-DEVICE-PARTITION-TYPE+
                +CL-DEVICE-PIPE-MAX-ACTIVE-RESERVATIONS+
                +CL-DEVICE-PIPE-MAX-PACKET-SIZE+
                +CL-DEVICE-PLATFORM+
                +CL-DEVICE-PREFERRED-GLOBAL-ATOMIC-ALIGNMENT+
                +CL-DEVICE-PREFERRED-INTEROP-USER-SYNC+
                +CL-DEVICE-PREFERRED-LOCAL-ATOMIC-ALIGNMENT+
                +CL-DEVICE-PREFERRED-PLATFORM-ATOMIC-ALIGNMENT+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-CHAR+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-SHORT+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-INT+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-LONG+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-FLOAT+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-DOUBLE+
                +CL-DEVICE-PREFERRED-VECTOR-WIDTH-HALF+
                +CL-DEVICE-PRINTF-BUFFER-SIZE+
                +CL-DEVICE-PROFILE+
                +CL-DEVICE-PROFILING-TIMER-RESOLUTION+
                +CL-DEVICE-QUEUE-ON-DEVICE-MAX-SIZE+
                +CL-DEVICE-QUEUE-ON-DEVICE-PREFERRED-SIZE+
                +CL-DEVICE-QUEUE-ON-DEVICE-PROPERTIES+
                +CL-DEVICE-QUEUE-ON-HOST-PROPERTIES+
                +CL-DEVICE-REFERENCE-COUNT+
                +CL-DEVICE-SINGLE-FP-CONFIG+
                +CL-DEVICE-SPIR-VERSIONS+
                ;; +CL-DEVICE-SUBGROUP-INDEPENDENT-FORWARD-PROGRESS+
                +CL-DEVICE-SVM-CAPABILITIES+
                +CL-DEVICE-TERMINATE-CAPABILITY-KHR+
                +CL-DEVICE-TYPE+
                +CL-DEVICE-VENDOR+
                +CL-DEVICE-VENDOR-ID+
                +CL-DEVICE-VERSION+
                +CL-DRIVER-VERSION+)))
    (loop
       for did in (cl-get-device-ids platform-id device-type)
       collecting
         (cons (cons :device-id did)
               (loop
                  for sym in syms
                  for val in vals
                  appending
                    (handler-case (list (cons sym (cl-get-device-info did val)))
                      (error () NIL)))))))
