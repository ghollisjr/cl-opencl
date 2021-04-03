(in-package :cl-opencl)

(defun cl-get-platform-ids ()
  (with-foreign-object (nplatforms :uint)
    (clgetplatformids 0 +NULL+ nplatforms)
    (let* ((nplats (mem-ref nplatforms :uint)))
      (with-foreign-object (platform-ids 'cl-platform-id nplats)
        (let* ((err-code
                (clgetplatformids (mem-ref nplatforms :uint)
                                  platform-ids
                                  +NULL+)))
          (when (= err-code +CL-SUCCESS+)
            (loop
               for i below nplats
               collecting (mem-aref platform-ids
                                    'cl-platform-id
                                    i))))))))

(defun cl-get-device-ids (platform-id platform-type)
  (with-foreign-object (ndevices :uint)
    (clgetdeviceids platform-id platform-type 0 +NULL+ ndevices)
    (let* ((nplats (mem-ref ndevices :uint)))
      (with-foreign-object (device-ids 'cl-device-id nplats)
        (let* ((err-code
                (clgetdeviceids platform-id
                                platform-type
                                (mem-ref ndevices :uint)
                                device-ids
                                cl-opencl-cffi:+NULL+)))
          (when (= err-code +CL-SUCCESS+)
            (loop
               for i below nplats
               collecting (mem-aref device-ids 'cl-device-id i))))))))
