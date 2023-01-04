(let ((test-parser
        (make-instance 'parser)))
  (with-input-from-string (stream "my LN expression")
    (format t "~%~%pars-stream example~%~a~%"
            (pars-stream test-parser stream))))
