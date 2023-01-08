(defun get-nesting (word current-nesting)
  (cond
    ((string= word "[")
     (values (1+ current-nesting) current-nesting))

    ((string= word "]")
     (values (1- current-nesting) (1- current-nesting)))

    (t
     (values current-nesting current-nesting))))

(defun print-it (tree)
  (loop for element across tree do
    (format t "~a~a~%"
            (print-it-get-nesting-string (first element))
            (second element))))

(defun print-it-get-nesting-string (nesting-degree)
  (loop with result = (make-array 0
                                  :fill-pointer 0
                                  :adjustable t
                                  :element-type 'character)
        repeat nesting-degree do
          (vector-push-extend #\Tab result)
        finally (return-from print-it-get-nesting-string result)))

(defun make-it-element (word nesting-degree)
  (list nesting-degree word))
