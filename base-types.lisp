(in-package :cl-opencl-cffi)

(progn
  (defctype cl-char :char)
  (defctype cl-uchar :uchar)
  (defctype cl-short :short)
  (defctype cl-ushort :ushort)
  (defctype cl-int :int)
  (defctype cl-uint :uint)
  (defctype cl-long :long)
  (defctype cl-ulong :ulong)
  ;; (defctype cl-half "cl_half")
  (defctype cl-float :float)
  (defctype cl-double :double))
