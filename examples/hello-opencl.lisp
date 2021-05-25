(require 'cl-opencl)
(in-package :cl-opencl)

;;; A simple hello world type of program
(defun hello-opencl ()
  "Demonstrate OpenCL API"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+))))
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
                              :flags
                              ;; technically +CL-MEM-COPY-HOST-PTR+ is
                              ;; automatically included whenever data
                              ;; is supplied, but you can still supply
                              ;; it
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
                              :flags +CL-MEM-READ-WRITE+ ; default value
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
