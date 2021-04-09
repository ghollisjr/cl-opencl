(defsystem cl-opencl
  :serial t
  :description "CFFI for OpenCL and Lisp wrapper API"
  :license "Public Domain"
  :author "Gary Hollis"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :components
  ((:file "package")
   (:file "base-types")
   (:cffi-grovel-file "grovel")
   (:file "cl-opencl-cffi")
   (:file "cl-opencl")))
