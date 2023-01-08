(multiple-value-bind (updated-nesting current-nesting) (get-nesting "[" 0)
  (format t "~a ~a~%" updated-nesting current-nesting))
