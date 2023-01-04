(let ((test-parser
        (make-instance 'parser
                       :buffer-word "This is a word."
                       :buffer-delimiter ",. ")))
  (format t "~%~%print-object example~%~a~%" test-parser))
