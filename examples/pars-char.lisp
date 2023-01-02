(let ((my-parser (make-instance 'parser
                                :buffer-delimiter (vector #\Space
                                                          #\Tab
                                                          #\Newline)))
      (my-line
        "This is a line with spaces
This is a line with	tabs
And, this is a newline
"))
  (format t "~%~%pars-char example~%~%")
  (loop for c across my-line do
    (setf my-parser (pars-char my-parser c)))
  (format t "~a~%" (parser-buffer-expression my-parser)))
