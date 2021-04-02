(defun format-forms (forms-list)
  (loop
     for f in forms-list
     for header = (subseq f 0 3)
     for footer = (subseq f 3)
     collecting `(,@header
                  ,@(mapcar (lambda (lst)
                              (list (second lst)
                                    (first lst)))
                            footer))))
