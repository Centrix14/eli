(let ((test-parser
        (make-instance 'parser)))
  (with-input-from-string (stream "abra cadabra")
    (format t "~a~%"
            (pars-stream test-parser stream))))
