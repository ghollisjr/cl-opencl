(in-package :cl-opencl-cffi)

(defparameter +NULL+ (cffi:null-pointer))

(define-foreign-library opencl
    (:unix (:or "libOpenCL.so")))

(use-foreign-library opencl)
;; platform API begins on line 933 of cl.h

(defcfun "clGetPlatformIDs" cl-int
  (num-entries cl-uint)
  (platforms :pointer)
  (nplatforms :pointer))

(defcfun "clGetDeviceIDs" cl-int
  (platform cl-platform-id)
  (platform-type :int)
  (num-entries cl-uint)
  (devices :pointer)
  (ndevices :pointer))
