(in-package :cl-opencl)

(defun cl-get-platform-ids ()
  (with-foreign-object (nplatforms :uint)
    (clgetplatformids 0 cl-opencl-cffi:+NULL+ nplatforms)
    (let* ((nplats (mem-ref nplatforms :uint)))
      (with-foreign-object (platform-ids 'cl-opencl-cffi::cl-platform-id nplats)
        (clgetplatformids (mem-ref nplatforms :uint) platform-ids cl-opencl-cffi:+NULL+)
        (loop
           for i below nplats
           collecting (mem-aref platform-ids 'cl-opencl-cffi::cl-platform-id i))))))

(defun cl-get-device-ids (platform-id platform-type)
  (with-foreign-object (ndevices :uint)
    (clgetdeviceids platform-id platform-type 0 cl-opencl-cffi:+NULL+ ndevices)
    (let* ((nplats (mem-ref ndevices :uint)))
      (with-foreign-object (device-ids 'cl-opencl-cffi::cl-device-id nplats)
        (clgetdeviceids platform-id
                        platform-type
                        (mem-ref ndevices :uint)
                        device-ids
                        cl-opencl-cffi:+NULL+)
        (loop
           for i below nplats
           collecting (mem-aref device-ids 'cl-opencl-cffi::cl-device-id i))))))
