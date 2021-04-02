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
  ;; would do something like this if half-floats were supported
  ;; (defctype cl-half :half)
  (defctype cl-float :float)
  (defctype cl-double :double))

(progn
  (defctype cl-gluint :uint)
  (defctype cl-glint :int)
  (defctype cl-glenum :uint))
