(defsystem cl-opencl
  :serial t
  :description "CFFI wrapper for OpenCL and higher level Lisp API"
  :license "Public Domain"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :components
  ((:file "package")
   (:file "base-types")
   (:cffi-grovel-file "grovel")
   (:file "cl-opencl-cffi")
   (:file "cl-opencl")))
