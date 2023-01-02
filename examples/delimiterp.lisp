(let ((my-parser (make-instance 'parser
                                :buffer-delimiter " ")))
  (format t "~%~%delimiterp example~%~a~%" (delimiterp my-parser #\Space)))
