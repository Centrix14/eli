(let ((test-parser
	   (make-instance 'parser
					  :buffer-delimiter " "))
	  (test-string "abra cadabra babra "))
  (loop for char across test-string do
	   (setf test-parser (pars-char test-parser char))))
