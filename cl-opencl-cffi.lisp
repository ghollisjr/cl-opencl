(in-package :cl-opencl-cffi)

(defparameter +NULL+ (cffi:null-pointer))

(define-foreign-library opencl
  (:unix (:or "libOpenCL.so")))

(use-foreign-library opencl)

;;; CL/cl.h
;; platform API
(defcfun "clGetPlatformIDs" cl-int
  (num-entries cl-uint)
  (platforms :pointer)
  (nplatforms :pointer))

(defcfun "clGetPlatformInfo" cl-int
  (platform cl-platform-id)
  (param-name cl-platform-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Device APIs
(defcfun "clGetDeviceIDs" cl-int
  (platform cl-platform-id)
  (platform-type :int)
  (num-entries cl-uint)
  (devices :pointer)
  (ndevices :pointer))

(defcfun "clGetDeviceInfo" cl-int
  (device cl-device-id)
  (param-name cl-device-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clCreateSubDevices" cl-int
  (in-device cl-device-id)
  (properties :pointer)
  (num-devices cl-uint)
  (out-devices :pointer)
  (num-devices-ret :pointer))

(defcfun "clRetainDevice" cl-int
  (device cl-device-id))

(defcfun "clReleaseDevice" cl-int
  (device cl-device-id))

(defcfun "clSetDefaultDeviceCommandQueue" cl-int
  (context cl-context)
  (device cl-device-id)
  (command-queue cl-command-queue))

(defcfun "clGetDeviceAndHostTimer" cl-int
  (device cl-device-id)
  (device-timestamp :pointer)
  (host-timestamp :pointer))

(defcfun "clGetHostTimer" cl-int
  (device cl-device-id)
  (host-timestamp :pointer))

;; Context APIs
(defcfun "clCreateContext" cl-context
  (properties :pointer)
  (num-devices cl-uint)
  (devices :pointer)
  (pfn-notify :pointer)
  (user-data :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateContextFromType" cl-context
  (properties :pointer)
  (device-type cl-device-type)
  (pfn-notify :pointer)
  (user-data :pointer)
  (errcode-ret :pointer))

(defcfun "clRetainContext" cl-int
  (context cl-context))

(defcfun "clReleaseContext" cl-int
  (context cl-context))

;; Command Queue APIs
(defcfun "clCreateCommandQueueWithProperties"
    cl-command-queue
  (context cl-context)
  (device cl-device-id)
  (properties :pointer)
  (errcode-ret :pointer))

(defcfun "clRetainCommandQueue" cl-int
  (command-queue cl-command-queue))

(defcfun "clReleaseCommandQueue" cl-int
  (command-queue cl-command-queue))

(defcfun "clGetCommandQueueInfo" cl-int
  (command-queue cl-command-queue)
  (param-name cl-command-queue-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Memory Object APIs
(defcfun "clCreateBuffer" cl-mem
  (context cl-context)
  (flags cl-mem-flags)
  (size size-t)
  (host-ptr :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateSubBuffer" cl-mem
  (buffer cl-mem)
  (flags cl-mem-flags)
  (buffer-create-type cl-buffer-create-type)
  (buffer-create-info :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateImage" cl-mem
  (context cl-context)
  (flags cl-mem-flags)
  (image-format :pointer)
  (image-desc :pointer)
  (host-ptr :pointer)
  (errcode-ret :pointer))

(defcfun "clCreatePipe" cl-mem
  (context cl-context)
  (flags cl-mem-flags)
  (pipe-packet-size cl-uint)
  (pipe-max-packets cl-uint)
  (properties :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateBufferWithProperties" cl-mem
  (context cl-context)
  (properties :pointer)
  (flags cl-mem-flags)
  (size size-t)
  (host-ptr :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateImageWithProperties" cl-mem
  (context cl-context)
  (properties :pointer)
  (flags cl-mem-flags)
  (image-format :pointer)
  (image-desc :pointer)
  (host-ptr :pointer)
  (errcode-ret :pointer))

(defcfun "clRetainMemObject" cl-int
  (memobj cl-mem))

(defcfun "clReleaseMemObject" cl-int
  (memobj cl-mem))

(defcfun "clGetSupportedImageFormats" cl-int
  (context cl-context)
  (flags cl-mem-flags)
  (image-type cl-mem-object-type)
  (num-entries cl-uint)
  (image-formats :pointer)
  (num-image-formats :pointer))

(defcfun "clGetMemObjectInfo" cl-int
  (memobj cl-mem)
  (param-name cl-mem-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetImageInfo" cl-int
  (image cl-mem)
  (param-name cl-image-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetPipeInfo" cl-int
  (pipe cl-mem)
  (param-name cl-pipe-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clSetMemObjectDestructorCallback" cl-int
  (memobj cl-mem)
  (pfn-notify :pointer)
  (user-data :pointer))

;; SVM Allocation APIs
(defcfun "clSVMAlloc" :pointer
  (context cl-context)
  (flags cl-svm-mem-flags)
  (size size-t)
  (alignment cl-uint))

(defcfun "clSVMFree" :void
  (context cl-context)
  (svm-pointer :pointer))

;; Sampler APIs
(defcfun "clCreateSamplerWithiProperties" cl-sampler
  (context cl-context)
  (sampler-properties :pointer)
  (errcode-ret :pointer))

(defcfun "clRetainSampler" cl-int
  (sampler cl-sampler))

(defcfun "clReleaseSampler" cl-int
  (sampler cl-sampler))

(defcfun "clGetSamplerInfo" cl-int
  (sampler cl-sampler)
  (param-name cl-sampler-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Program Object APIs
(defcfun "clCreateProgramWithSource" cl-program
  (context cl-context)
  (count cl-uint)
  (strings :pointer)
  (lengths :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateProgramWithBinary" cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list :pointer)
  (lengths :pointer)
  (binaries :pointer)
  (binary-status :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateProgramWithBuiltInKernels" cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list :pointer)
  (kernel-names :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateProgramWithIL" cl-program
  (context cl-context)
  (il :pointer)
  (length size-t)
  (errcode-ret :pointer))

(defcfun "clRetainProgram" cl-int
  (program cl-program))

(defcfun "clReleaseProgram" cl-int
  (program cl-program))

(defcfun "clBuildProgram" cl-int
  (program cl-program)
  (num-devices cl-uint)
  (device-list :pointer)
  (options :pointer)
  (pfn-notify :pointer)
  (user-data :pointer))

(defcfun "clCompileProgram" cl-int
  (program cl-program)
  (num-devices cl-uint)
  (device-list :pointer)
  (options :pointer)
  (num-input-headers cl-uint)
  (input-headers :pointer)
  (header-include-names :pointer)
  (pfn-notify :pointer)
  (user-data :pointer))

(defcfun "clLinkProgram" cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list :pointer)
  (options :pointer)
  (num-input-programs cl-uint)
  (input-programs :pointer)
  (pfn-notify :pointer)
  (user-data :pointer)
  (errcode-ret :pointer))

(defcfun "clSetProgramReleaseCallback" cl-int
  (program cl-program)
  (pfn-notify :pointer)
  (user-data :pointer))

(defcfun "clSetProgramSpecializationConstant" cl-int
  (program cl-program)
  (spec-id cl-uint)
  (spec-size size-t)
  (spec-value :pointer))

(defcfun "clUnloadPlatformCompiler" cl-int
  (platform cl-platform-id))

(defcfun "clGetProgramInfo" cl-int
  (program cl-program)
  (param-name cl-program-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetProgramBuildInfo" cl-int
  (program cl-program)
  (device cl-device-id)
  (param-name cl-program-build-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Kernel Object APIs
(defcfun "clCreateKernel" cl-kernel
  (program cl-program)
  (kernel-name :pointer)
  (errcode-ret :pointer))

(defcfun "clCreateKernelsInProgram" cl-int
  (program cl-program)
  (num-kernels cl-uint)
  (kernels :pointer)
  (num-kernels-ret :pointer))

(defcfun "clCloneKernel" cl-kernel
  (source-kernel cl-kernel)
  (errcode-ret :pointer))

(defcfun "clRetainKernel" cl-int
  (kernel cl-kernel))

(defcfun "clReleaseKernel" cl-int
  (kernel cl-kernel))

(defcfun "clSetKernelArg" cl-int
  (kernel cl-kernel)
  (arg-index cl-uint)
  (arg-size size-t)
  (arg-value :pointer))

(defcfun "clSetKernelArgSVMPointer" cl-int
  (kernel cl-kernel)
  (arg-index cl-uint)
  (arg-value :pointer))

(defcfun "clSetKernelExecInfo" cl-int
  (kernel cl-kernel)
  (param-name cl-kernel-exec-info)
  (param-value-size size-t)
  (param-value :pointer))

(defcfun "clGetKernelInfo" cl-int
  (kernel cl-kernel)
  (param-name cl-kernel-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetKernelArgInfo" cl-int
  (kernel cl-kernel)
  (arg-indx cl-uint)
  (param-name cl-kernel-arg-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetKernelWorkGroupInfo" cl-int
  (kernel cl-kernel)
  (device cl-device-id)
  (param-name cl-kernel-work-group-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clGetKernelSubGroupInfo" cl-int
  (kernel cl-kernel)
  (device cl-device-id)
  (param-name cl-kernel-sub-group-info)
  (input-value-size size-t)
  (input-value :pointer)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Event Object APIs
(defcfun "clWaitForEvents" cl-int
  (num-events cl-uint)
  (event-list :pointer))

(defcfun "clGetEventInfo" cl-int
  (event cl-event)
  (param-name cl-event-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

(defcfun "clCreateUserEvent" cl-event
  (context cl-context)
  (errcode-ret :pointer))

(defcfun "clRetainEvent" cl-int
  (event cl-event))

(defcfun "clReleaseEvent" cl-int
  (event cl-event))

(defcfun "clSetUserEventStatus" cl-int
  (event cl-event)
  (execution-status cl-int))

(defcfun "clSetEventCallback" cl-int
  (event cl-event)
  (command-exec-callback-type cl-int)
  (pfn-notify :pointer)
  (user-data :pointer))

;; Profiling APIs
(defcfun "clGetEventProfilingInfo" cl-int
  (event cl-event)
  (param-name cl-profiling-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret :pointer))

;; Flush and Finish APIs
(defcfun "clFlush" cl-int
  (command-queue cl-command-queue))

(defcfun "clFinish" cl-int
  (command-queue cl-command-queue))

;; Enqueued Commands APIs
(defcfun "clEnqueueReadBuffer" cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-read cl-bool)
  (offset size-t)
  (size size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueReadBufferRect" cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-read cl-bool)
  (host-offset :pointer)
  (region :pointer)
  (buffer-row-pitch size-t)
  (buffer-slice-pitch size-t)
  (host-row-pitch size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueWriteBuffer" cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-write cl-bool)
  (offset size-t)
  (size size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueWriteBufferRect" cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-write cl-bool)
  (buffer-offset :pointer)
  (host-offset :pointer)
  (region :pointer)
  (buffer-row-pitch size-t)
  (buffer-slice-pitch size-t)
  (host-row-pitch size-t)
  (host-slice-pitch size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueFillBuffer" cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (pattern :pointer)
  (pattern-size size-t)
  (offset size-t)
  (size size-t)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueCopyBuffer" cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-buffer cl-mem)
  (src-offset size-t)
  (dst-offset size-t)
  (size size-t)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueCopyBufferRect" cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-buffer cl-mem)
  (src-origin :pointer)
  (dst-origin :pointer)
  (region :pointer)
  (src-row-pitch size-t)
  (src-slice-pitch size-t)
  (dst-row-pitch size-t)
  (dst-slice-pitch size-t)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueReadImage" cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (blocking-read cl-bool)
  (origin :pointer)
  (region :pointer)
  (row-pitch size-t)
  (slice-pitch size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueWriteImage" cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (blocking-write cl-bool)
  (origin :pointer)
  (region :pointer)
  (input-row-pitch size-t)
  (input-slice-pitch size-t)
  (ptr :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueFillImage" cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (fill-color :pointer)
  (origin :pointer)
  (region :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueCopyImage" cl-int
  (command-queue cl-command-queue)
  (src-image cl-mem)
  (dst-image cl-mem)
  (src-origin :pointer)
  (dst-origin :pointer)
  (region :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueCopyImageToBuffer" cl-int
  (command-queue cl-command-queue)
  (src-image cl-mem)
  (dst-buffer cl-mem)
  (src-origin :pointer)
  (region :pointer)
  (dst-offset size-t)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(defcfun "clEnqueueCopyBufferToImage" cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-image cl-mem)
  (src-offset size-t)
  (dst-origin :pointer)
  (region :pointer)
  (num-events-in-wait-list cl-uint)
  (event-wait-list :pointer)
  (event :pointer))

(DEFCFUN "clEnqueueMapBuffer"
    :POINTER
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (BUFFER CL-MEM)
  (BLOCKING-MAP CL-BOOL)
  (MAP-FLAGS CL-MAP-FLAGS)
  (OFFSET SIZE-T)
  (SIZE SIZE-T)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clEnqueueMapImage"
    :POINTER
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (IMAGE CL-MEM)
  (BLOCKING-MAP CL-BOOL)
  (MAP-FLAGS CL-MAP-FLAGS)
  (ORIGIN :POINTER)
  (REGION :POINTER)
  (IMAGE-ROW-PITCH :POINTER)
  (IMAGE-SLICE-PITCH :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clEnqueueUnmapMemObject"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (MEMOBJ CL-MEM)
  (MAPPED-PTR :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueMigrateMemObjects"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-MEM-OBJECTS CL-UINT)
  (MEM-OBJECTS :POINTER)
  (FLAGS CL-MEM-MIGRATION-FLAGS)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueNDRangeKernel"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (KERNEL CL-KERNEL)
  (WORK-DIM CL-UINT)
  (GLOBAL-WORK-OFFSET :POINTER)
  (GLOBAL-WORK-SIZE :POINTER)
  (LOCAL-WORK-SIZE :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueNativeKernel"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (USER-FUNC :POINTER)
  (ARGS :POINTER)
  (CB-ARGS SIZE-T)
  (NUM-MEM-OBJECTS CL-UINT)
  (MEM-LIST :POINTER)
  (ARGS-MEM-LOC :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueMarkerWithWaitList"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueBarrierWithWaitList"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMFree"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-SVM-POINTERS CL-UINT)
  (SVM-POINTERS[] :POINTER)
  (PFN-FREE-FUNC :POINTER)
  (USER-DATA :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMMemcpy"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (BLOCKING-COPY CL-BOOL)
  (DST-PTR :POINTER)
  (src-ptr :POINTER)
  (SIZE SIZE-T)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMMemFill"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (SVM-PTR :POINTER)
  (pattern :POINTER)
  (PATTERN-SIZE SIZE-T)
  (SIZE SIZE-T)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMMap"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (BLOCKING-MAP CL-BOOL)
  (FLAGS CL-MAP-FLAGS)
  (SVM-PTR :POINTER)
  (SIZE SIZE-T)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMUnmap"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (SVM-PTR :POINTER)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueSVMMigrateMem"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-SVM-POINTERS CL-UINT)
  (SVM-POINTERS :POINTER)
  (SIZES :POINTER)
  (FLAGS CL-MEM-MIGRATION-FLAGS)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))
(DEFCFUN "clGetExtensionFunctionAddressForPlatform"
    :POINTER
  (PLATFORM CL-PLATFORM-ID)
  (FUNC-NAME :POINTER))
(DEFCFUN "clSetCommandQueueProperty"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (PROPERTIES CL-COMMAND-QUEUE-PROPERTIES)
  (ENABLE CL-BOOL)
  (OLD-PROPERTIES :POINTER))
(DEFCFUN "clCreateImage2D"
    CL-MEM
  (CONTEXT CL-CONTEXT)
  (FLAGS CL-MEM-FLAGS)
  (IMAGE-FORMAT :POINTER)
  (IMAGE-WIDTH SIZE-T)
  (IMAGE-HEIGHT SIZE-T)
  (IMAGE-ROW-PITCH SIZE-T)
  (HOST-PTR :POINTER)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clCreateImage3D"
    CL-MEM
  (CONTEXT CL-CONTEXT)
  (FLAGS CL-MEM-FLAGS)
  (IMAGE-FORMAT :POINTER)
  (IMAGE-WIDTH SIZE-T)
  (IMAGE-HEIGHT SIZE-T)
  (IMAGE-DEPTH SIZE-T)
  (IMAGE-ROW-PITCH SIZE-T)
  (IMAGE-SLICE-PITCH SIZE-T)
  (HOST-PTR :POINTER)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clEnqueueMarker"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (EVENT :POINTER))
(DEFCFUN "clEnqueueWaitForEvents"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (NUM-EVENTS CL-UINT)
  (EVENT-LIST :POINTER))
(DEFCFUN "clEnqueueBarrier"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE))
(DEFCFUN "clUnloadCompiler"
    CL-INT)
(DEFCFUN "clGetExtensionFunctionAddress"
    :POINTER
  (FUNC-NAME :POINTER))
(DEFCFUN "clCreateCommandQueue"
    CL-COMMAND-QUEUE
  (CONTEXT CL-CONTEXT)
  (DEVICE CL-DEVICE-ID)
  (PROPERTIES CL-COMMAND-QUEUE-PROPERTIES)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clCreateSampler"
    CL-SAMPLER
  (CONTEXT CL-CONTEXT)
  (NORMALIZED-COORDS CL-BOOL)
  (ADDRESSING-MODE CL-ADDRESSING-MODE)
  (FILTER-MODE CL-FILTER-MODE)
  (ERRCODE-RET :POINTER))
(DEFCFUN "clEnqueueTask"
    CL-INT
  (COMMAND-QUEUE CL-COMMAND-QUEUE)
  (KERNEL CL-KERNEL)
  (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
  (EVENT-WAIT-LIST :POINTER)
  (EVENT :POINTER))

;;; CL/cl_gl.h
;; API
(progn
  (DEFCFUN "clCreateFromGLBuffer"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (BUFOBJ CL-GLUINT)
    (ERRCODE-RET :POINTER))
  (DEFCFUN "clCreateFromGLTexture"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (TARGET CL-GLENUM)
    (MIPLEVEL CL-GLINT)
    (TEXTURE CL-GLUINT)
    (ERRCODE-RET :POINTER))
  (DEFCFUN "clCreateFromGLRenderbuffer"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (RENDERBUFFER CL-GLUINT)
    (ERRCODE-RET :POINTER))
  (DEFCFUN "clGetGLObjectInfo"
      CL-INT
    (MEMOBJ CL-MEM)
    (GL-OBJECT-TYPE :POINTER)
    (GL-OBJECT-NAME :POINTER))
  (DEFCFUN "clGetGLTextureInfo"
      CL-INT
    (MEMOBJ CL-MEM)
    (PARAM-NAME CL-GL-TEXTURE-INFO)
    (PARAM-VALUE-SIZE SIZE-T)
    (PARAM-VALUE :POINTER)
    (PARAM-VALUE-SIZE-RET :POINTER))
  (DEFCFUN "clEnqueueAcquireGLObjects"
      CL-INT
    (COMMAND-QUEUE CL-COMMAND-QUEUE)
    (NUM-OBJECTS CL-UINT)
    (MEM-OBJECTS :POINTER)
    (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
    (EVENT-WAIT-LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueReleaseGLObjects"
      CL-INT
    (COMMAND-QUEUE CL-COMMAND-QUEUE)
    (NUM-OBJECTS CL-UINT)
    (MEM-OBJECTS :POINTER)
    (NUM-EVENTS-IN-WAIT-LIST CL-UINT)
    (EVENT-WAIT-LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clCreateFromGLTexture2D"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (TARGET CL-GLENUM)
    (MIPLEVEL CL-GLINT)
    (TEXTURE CL-GLUINT)
    (ERRCODE-RET :POINTER))
  (DEFCFUN "clCreateFromGLTexture3D"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (TARGET CL-GLENUM)
    (MIPLEVEL CL-GLINT)
    (TEXTURE CL-GLUINT)
    (ERRCODE-RET :POINTER))
  (DEFCFUN "clGetGLContextInfoKHR"
      CL-INT
    (PROPERTIES :POINTER)
    (PARAM-NAME CL-GL-CONTEXT-INFO)
    (PARAM-VALUE-SIZE SIZE-T)
    (PARAM-VALUE :POINTER)
    (PARAM-VALUE-SIZE-RET :POINTER)))

;;; CL/cl_gl_ext.h
(defcfun "clCreateEventFromGLsyncKHR" cl-event
  (context cl-context)
  (cl-GLsync cl-GLsync)
  (errcode-ret :pointer))

;;; CL/cl_ext.h
(progn
  (DEFCFUN "clSetMemObjectDestructorAPPLE"
      CL-INT
    (MEMOBJ CL-MEM)
    (PFN_NOTIFY :POINTER)
    (USER_DATA :POINTER))
  (DEFCFUN "clLogMessagesToSystemLogAPPLE"
      :VOID
    (ERRSTR :POINTER)
    (PRIVATE_INFO :POINTER)
    (CB SIZE-T)
    (USER_DATA :POINTER))
  (DEFCFUN "clLogMessagesToStdoutAPPLE"
      :VOID
    (ERRSTR :POINTER)
    (PRIVATE_INFO :POINTER)
    (CB SIZE-T)
    (USER_DATA :POINTER))
  (DEFCFUN "clLogMessagesToStderrAPPLE"
      :VOID
    (ERRSTR :POINTER)
    (PRIVATE_INFO :POINTER)
    (CB SIZE-T)
    (USER_DATA :POINTER))
  (DEFCFUN "clIcdGetPlatformIDsKHR"
      CL-INT
    (NUM_ENTRIES CL-UINT)
    (PLATFORMS :POINTER)
    (NUM_PLATFORMS :POINTER))
  (DEFCFUN "clCreateProgramWithILKHR"
      CL-PROGRAM
    (CONTEXT CL-CONTEXT)
    (IL :POINTER)
    (LENGTH SIZE-T)
    (ERRCODE_RET :POINTER))
  (DEFCFUN "clTerminateContextKHR"
      CL-INT
    (CONTEXT CL-CONTEXT))
  (DEFCFUN "clCreateCommandQueueWithPropertiesKHR"
      CL-COMMAND-QUEUE
    (CONTEXT CL-CONTEXT)
    (DEVICE CL-DEVICE-ID)
    (PROPERTIES :POINTER)
    (ERRCODE_RET :POINTER))
  (DEFCFUN "clReleaseDeviceEXT"
      CL-INT
    (DEVICE CL-DEVICE-ID))
  (DEFCFUN "clRetainDeviceEXT"
      CL-INT
    (DEVICE CL-DEVICE-ID))
  (DEFCFUN "clCreateSubDevicesEXT"
      CL-INT
    (IN_DEVICE CL-DEVICE-ID)
    (PROPERTIES :POINTER)
    (NUM_ENTRIES CL-UINT)
    (OUT_DEVICES :POINTER)
    (NUM_DEVICES :POINTER))
  (DEFCFUN "clEnqueueMigrateMemObjectEXT"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (NUM_MEM_OBJECTS CL-UINT)
    (MEM_OBJECTS :POINTER)
    (FLAGS CL-MEM-MIGRATION-FLAGS-EXT)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clGetDeviceImageInfoQCOM"
      CL-INT
    (DEVICE CL-DEVICE-ID)
    (IMAGE_WIDTH SIZE-T)
    (IMAGE_HEIGHT SIZE-T)
    (IMAGE_FORMAT :POINTER)
    (PARAM_NAME CL-IMAGE-PITCH-INFO-QCOM)
    (PARAM_VALUE_SIZE SIZE-T)
    (PARAM_VALUE :POINTER)
    (PARAM_VALUE_SIZE_RET :POINTER))
  (DEFCFUN "clEnqueueAcquireGrallocObjectsIMG"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (NUM_OBJECTS CL-UINT)
    (MEM_OBJECTS :POINTER)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueReleaseGrallocObjectsIMG"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (NUM_OBJECTS CL-UINT)
    (MEM_OBJECTS :POINTER)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clGetKernelSubGroupInfoKHR"
      CL-INT
    (IN_KERNEL CL-KERNEL)
    (IN_DEVICE CL-DEVICE-ID)
    (PARAM_NAME CL-KERNEL-SUB-GROUP-INFO)
    (INPUT_VALUE_SIZE SIZE-T)
    (INPUT_VALUE :POINTER)
    (PARAM_VALUE_SIZE SIZE-T)
    (PARAM_VALUE :POINTER)
    (PARAM_VALUE_SIZE_RET :POINTER))
  (DEFCFUN "clImportMemoryARM"
      CL-MEM
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-MEM-FLAGS)
    (PROPERTIES :POINTER)
    (MEMORY :POINTER)
    (SIZE SIZE-T)
    (ERRCODE_RET :POINTER))
  (DEFCFUN "clSVMAllocARM"
      :POINTER
    (CONTEXT CL-CONTEXT)
    (FLAGS CL-SVM-MEM-FLAGS-ARM)
    (SIZE SIZE-T)
    (ALIGNMENT CL-UINT))
  (DEFCFUN "clSVMFreeARM"
      :VOID
    (CONTEXT CL-CONTEXT)
    (SVM_POINTER :POINTER))
  (DEFCFUN "clEnqueueSVMFreeARM"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (NUM_SVM_POINTERS CL-UINT)
    (SVM_POINTERS :POINTER)
    (PFN_FREE_FUNC :POINTER)
    (USER_DATA :POINTER)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueSVMMemcpyARM"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (BLOCKING_COPY CL-BOOL)
    (DST_PTR :POINTER)
    (SRC_PTR :POINTER)
    (SIZE SIZE-T)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueSVMMemFillARM"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (SVM_PTR :POINTER)
    (PATTERN :POINTER)
    (PATTERN_SIZE SIZE-T)
    (SIZE SIZE-T)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueSVMMapARM"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (BLOCKING_MAP CL-BOOL)
    (FLAGS CL-MAP-FLAGS)
    (SVM_PTR :POINTER)
    (SIZE SIZE-T)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clEnqueueSVMUnmapARM"
      CL-INT
    (COMMAND_QUEUE CL-COMMAND-QUEUE)
    (SVM_PTR :POINTER)
    (NUM_EVENTS_IN_WAIT_LIST CL-UINT)
    (EVENT_WAIT_LIST :POINTER)
    (EVENT :POINTER))
  (DEFCFUN "clSetKernelArgSVMPointerARM"
      CL-INT
    (KERNEL CL-KERNEL)
    (ARG_INDEX CL-UINT)
    (ARG_VALUE :POINTER))
  (DEFCFUN "clSetKernelExecInfoARM"
      CL-INT
    (KERNEL CL-KERNEL)
    (PARAM_NAME CL-KERNEL-EXEC-INFO-ARM)
    (PARAM_VALUE_SIZE SIZE-T)
    (PARAM_VALUE :POINTER)))
