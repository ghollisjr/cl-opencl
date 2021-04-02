;; This is software that helped me generate the many vector union
;; types for the grovel file.

(defun lispify (symbol-or-string)
  "Converts symbol or string into a string and replaces all spaces and
  underscores with -, and convert to uppercase."
  (string-upcase (map 'string
		      (lambda (c)
                        (case c
                          (#\Space     #\-)
                          (#\_         #\-)
                          (otherwise   c)))
		      (string symbol-or-string))))

(defun genvecunions (ctype n)
  (let ((typesym (intern (lispify ctype))))
    (cons 'progn
          (loop
             for i = 2 then (* 2 i)
             while (<= i n)
             collecting
               (let* ((vtype (concatenate
                              'string
                              ctype
                              (format nil "~a" i)))
                      (vsym (intern (lispify vtype))))
                 `(cunion ,vsym ,vtype
                          (s "s" :type ,typesym :count ,i)))))))

(defun genvecunions-all ()
  (loop
     for type in
       (list "cl_char"
             "cl_uchar"
             "cl_short"
             "cl_ushort"
             "cl_half"
             "cl_uhalf"
             "cl_int"
             "cl_uint"
             "cl_long"
             "cl_ulong"
             "cl_float"
             "cl_double")
     collecting
       (genvecunions type 16)))
