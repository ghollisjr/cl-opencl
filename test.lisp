(require 'cl-opencl)
(in-package :cl-opencl)

;;; A simple hello world type of program
(defun hello-opencl ()
  "Demonstrate OpenCL API"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-GPU+)))
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
    (cl-build-program program (list dev)
                      :options "-cl-kernel-arg-info")
    (let* ((njobs 100)
           (kernel
            (cl-create-kernel program "hello"))
           (nbuf
            (cl-create-buffer context
                              (list +CL-MEM-READ-WRITE+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :uint
                              :data (list njobs)))
           (outbuf
            (cl-create-buffer context
                              +CL-MEM-READ-WRITE+
                              :size
                              (* njobs
                                 (foreign-type-size
                                  :uint))))
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
                                      (list :array :uint njobs)
                                      :blocking-p t)))
        (cl-release-kernel kernel)
        (cl-release-program program)
        (cl-release-mem-object outbuf)
        (cl-release-mem-object nbuf)
        (cl-release-context context)
        result))))

;;; Mandelbrot set example
;; Writing data to a PPM file
(defun write-ppm (pathname maxval width height 3D-pixel-array)
  "Writes PPM image pixel values in 3-D pixel array to file at
pathname."
  (let* ((nbytes (if (> maxval 255)
                     2
                     1))
         (buffer (make-array (* 3 ;; RGB
                                nbytes width height)
                             :element-type '(unsigned-byte 8)))
         (index 0))
    (loop
       for j below height
       do
         (loop
            for i below width
            do
              (loop
                 for k below 3
                 for x in (aref 3D-pixel-array i j 0)
                 do
                   (loop
                      for w below nbytes
                      for y = (floor x 256) then (mod x 256)
                      do (setf (aref buffer (1- (incf index)))
                               y)))))
    (with-open-file (file pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "P6~%~a ~a ~a~%"
              width height maxval))
    (with-open-file (file pathname
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))

      (write-sequence buffer
                      file)
      NIL)))

;; Generate Mandelbrot fractal picture.  Careful: It will destroy
;; whatever is located at picture-output-pathname.
(defun mandelbrot-cl (picture-output-pathname
                      &key
                        (max-iterations 250)
                        (width-height (cons 800 800))
                        (xrange (cons -2f0 2f0))
                        (yrange (cons -2f0 2f0)))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-GPU+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev))
         (width (car width-height))
         (height (cdr width-height))
         (program
          (cl-create-program-with-source
           context
           "#define BOUNDARY2 4.0f

__constant float nbits = 16.0f;

uint4 bitify(float4 f)
{
  const float scale = exp2(nbits)-1;
  return (uint4)((uint) round(f.s0*scale),
                 (uint) round(f.s1*scale),
                 (uint) round(f.s2*scale),
                 (uint) round(f.s3*scale));
}

float4 jet(float f)
{
  return (float4)(clamp(1.5f - fabs(4.0f * f - 3.0f),0.0f,1.0f),
                  clamp(1.5f - fabs(4.0f * f - 2.0f),0.0f,1.0f),
                  clamp(1.5f - fabs(4.0f * f - 1.0f),0.0f,1.0f),
                  1.0f);
}

__kernel
void mandelbrot(__global float* boundaries,
                __global unsigned* nxy,
                __global unsigned* niter,
                __write_only image2d_t bmp)
{
  const unsigned nx = nxy[0];
  const unsigned ny = nxy[1];
  const unsigned n = nx*ny;
  const int gid = get_global_id(0);
  if(gid < n) {
    float xlo = boundaries[0];
    float xhi = boundaries[1];
    float ylo = boundaries[2];
    float yhi = boundaries[3];
    //where are we
    const float dx = (xhi - xlo)/(nx-1);
    const float dy = (yhi - ylo)/(ny-1);
    const unsigned ix = gid%(nx);
    const unsigned iy = gid/(nx);
    const float x = xlo + dx*ix;
    const float y = ylo + dy*iy;
    float zx = 0;
    float zy = 0;
    int i = 0;
    for(i = 0; i <= *niter; ++i) {
      const float tmpx = x + zx*zx - zy*zy;
      const float tmpy = y + 2*zx*zy;
      zx = tmpx;
      zy = tmpy;
      const float n = zx*zx + zy*zy;
      if(n > (float) BOUNDARY2) {
        break;
      }
    }

    int2 xy = (int2)(ix,iy);
    const float f = ((float) i)/(*niter+1);
    write_imageui(bmp,xy,bitify(jet(f)));
    //write_imagef(bmp,xy,jet(f));
  }
}")))
    (cl-build-program program (list dev))
    (let* ((kernel
            (cl-create-kernel program "mandelbrot"))
           (boundaries
            (cl-create-buffer context
                              (list +CL-MEM-READ-ONLY+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :float
                              :data (list (car xrange)
                                          (cdr xrange)
                                          (car yrange)
                                          (cdr yrange))))
           (grid
            (cl-create-buffer context
                              (list +CL-MEM-READ-ONLY+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :uint
                              :data (list width
                                          height)))
           (niter
            (cl-create-buffer context
                              (list +CL-MEM-READ-ONLY+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :uint
                              :data (list max-iterations)))
           (image
            (cl-create-image context +CL-MEM-WRITE-ONLY+
                             :image-channel-data-type +CL-UNSIGNED-INT16+
                             :width width
                             :height height)))
      (cl-set-kernel-arg kernel 0 :value boundaries)
      (cl-set-kernel-arg kernel 1 :value grid)
      (cl-set-kernel-arg kernel 2 :value niter)
      (cl-set-kernel-arg kernel 3 :value image)
      (let* ((njobs (* width height))
             (nwork (min njobs
                         (cl-get-kernel-work-group-info
                          kernel dev
                          +CL-KERNEL-WORK-GROUP-SIZE+)))
             (nglobal (* nwork
                         (ceiling njobs nwork))))
        (cl-wait-and-release-events
         (list
          (cl-enqueue-ndrange-kernel queue kernel
                                     (list nglobal)
                                     (list nwork)))))
      (let* ((imdata
              (cl-enqueue-read-image queue image
                                     :blocking-p t)))
        (mapcar #'cl-release-mem-object
                (list image
                      niter
                      grid
                      boundaries))
        (cl-release-kernel kernel)
        (cl-release-program program)
        (cl-release-context context)
        (write-ppm picture-output-pathname
                   (- (* 256 256) 1)
                   width
                   height
                   imdata)))))
