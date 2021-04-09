cl-opencl is a CFFI project to provide access to the OpenCL C API to
Common Lisp software.

cl-opencl is in the public domain; use it for whatever.

OVERVIEW:

cl-opencl has two layers: The low-level CFFI API, and a high-level
Lisp wrapper.  The Lisp wrapper does not support functions marked as
deprecated, but those deprecated functions have almost always been
effectively included in the Lisp API due to flexible keyword
arguments.

The OpenGL and Direct3D intercommunication libraries are hoped to be
included in the Lisp wrapper.  It would be nice to make cl-opencl work
well with cl-opengl as found in Quicklisp.

The test.lisp file shows some examples of how to use the cl-opencl
Lisp wrapper.

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
