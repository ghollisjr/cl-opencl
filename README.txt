cl-opencl is a CFFI project to provide access to the OpenCL C API to
Common Lisp software.

cl-opencl is in the public domain; use it for whatever.

IMPLEMENTATION LIMITATIONS:

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
