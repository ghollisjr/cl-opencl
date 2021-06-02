cl-opencl is a CFFI project to provide access to the OpenCL C API to
Common Lisp software.

cl-opencl is in the public domain; use it for whatever.

OVERVIEW:

There are already OpenCL Common Lisp packages available via quicklisp:

* clocl (https://github.com/gos-k/oclcl)
* cl-oclapi (https://github.com/gos-k/cl-oclapi)

cl-oclapi supplies a manually-defined CFFI to OpenCL, and clocl builds
on this to provide a Lisp-like OpenCL kernel language.

This project was designed as an alternative strategy to the above two
libraries.  cl-opencl has two layers:

1. The low-level CFFI API made with the CFFI groveler.

2. A high-level Lisp wrapper that only uses Lisp data types as much as
   possible, converting between Lisp and foreign types and providing
   standard methods for dealing with foreign memory cleanup when
   needed.  For example, buffers can be read and written using arrays.

The Lisp wrapper does not support functions marked as deprecated, but
those deprecated functions have almost always been effectively
included in the Lisp API due to flexible keyword arguments.

NOTE: The cl-opencl-utils library located at

https://www.github.com/ghollisjr/cl-opencl-utils

provides a Lispified OpenCL C language with automatic function and
kernel definition management, whereas cl-opencl by itself just uses
strings containing OpenCL C kernel language code.  cl-opencl-utils
also provides utilities for map-reduce algorithms and convolutions,
for example.

The OpenGL and Direct3D intercommunication libraries are hoped to be
included in the Lisp wrapper.  It would be nice to make cl-opencl work
well with cl-opengl as found in Quicklisp.  At the moment, I've had
success integrating very basic private OpenGL CFFI Lisp libraries with
cl-opencl.

EXAMPLES:

The examples directory contains a number of examples, including a
Mandelbrot fractal PNM image generator.  Here's a trivial example that
sets the indices of an array using an OpenCL kernel on the first
available OpenCL device.

(defun hello-opencl ()
  "Demonstrate OpenCL API"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context
          (cl-create-context plat (list dev)))
         (program
          (cl-create-program-with-source
           context
           "__kernel
void hello(__global uint* n,
           __global uint* buf)
{
  int gid = get_global_id(0);
  if(gid < *n) {
    buf[gid] = gid;
  }
}")))
    (cl-build-program-with-log program (list dev)
                               :options "-cl-kernel-arg-info")
    (let* ((njobs 100)
           (kernel
            (cl-create-kernel program "hello"))
           (nbuf
            (cl-create-buffer context
                              ;; Note that +CL-MEM-COPY-HOST-PTR+ is
                              ;; automatically enabled when data is
                              ;; supplied, but there's no harm in
                              ;; adding it to flags explicitly
                              :flags
                              (list +CL-MEM-READ-WRITE+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :uint
                              :data (list njobs)))
           (outbuf
            ;; Manual size calculation:
            ;; 
            ;; (cl-create-buffer context
            ;;                   :flags
            ;;                   +CL-MEM-READ-WRITE+
            ;;                   :size
            ;;                   (* njobs
            ;;                      (foreign-type-size
            ;;                       :uint)))

            ;; More convenient automatic size calculation:
             (cl-create-buffer context
                               :flags
                               +CL-MEM-READ-WRITE+
                               :count njobs
                               :type :uint))
           (queue
            (cl-create-command-queue context dev))
           (nwork
            (cl-get-kernel-work-group-info
             kernel dev +CL-KERNEL-WORK-GROUP-SIZE+))
           (nglobal (* nwork
                       (ceiling njobs nwork))))
      (cl-set-kernel-arg kernel 0 :value nbuf)
      (cl-set-kernel-arg kernel 1 :value outbuf)
      (cl-wait-and-release-events
       (list
        (cl-enqueue-ndrange-kernel queue kernel
                                   (list nglobal)
                                   (list nwork))))
      (let* ((result
              (cl-enqueue-read-buffer queue outbuf
                                      :uint njobs
                                      :blocking-p t)))
        (cl-release-kernel kernel)
        (cl-release-program program)
        (cl-release-mem-object outbuf)
        (cl-release-mem-object nbuf)
        (cl-release-context context)
        result))))

To make understanding your specific platform easier, you can use the
following functions:

(describe-opencl-platforms) ==> a list of alists with all properties available
for all platforms

(describe-opencl-devices platform device-type) ==> a list of alists
with all properties available for all devices matching the type and
platform.

IMPLEMENTATION DETAILS:

* The vector types are implemented as unions with anonymous structs in
  C, and I have no idea how to make that work with CFFI.  I used
  cunion and picked the s-array union member so that at least you can
  use these types in Lisp and not lose any functionality, you just
  won't have the nice field names like x/y/z/etc.

* Half-precision floats are omitted from vector types as I couldn't
  get the groveler to work with them.

* Some constant values for infinity and not-a-number are omitted
  because the groveler fails to interpret them numerically and just
  reads strings "inf" or "nan" and complains that they're not numbers.

* Native kernels are not included in the high-level API at the moment,
  as I'm not entirely sure what the best way to enable them would look
  like.  It's still possible to use them with the CFFI function
  clEnqueueNativeKernel, you'll just have to manage foreign memory
  manually.
