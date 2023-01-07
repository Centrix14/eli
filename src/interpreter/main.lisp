(defun ln-eval-stream (stream)
  (let ((expression (make-array 0
                                :fill-pointer 0
                                :adjustable t
                                :element-type 'list))
        (word "")
        (nesting-degree 0)
        (separators (vector #\Space
                            #\Newline
                            #\Tab)))

    (setf word (read-next-word stream separators))
    (loop until (null word) do
      (multiple-value-bind (updated-nesting word-nesting)
          (get-nesting word nesting-degree)

        (vector-push-extend (make-it-element word word-nesting)
                            expression)
        (setf nesting-degree updated-nesting))

      (setf word (read-next-word stream separators)))

    (print-it expression)))
