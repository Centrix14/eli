(let ((str
        "atom while

rule [ while .seq:cond .seq:body ]
     [ rule [ while 1 .seq ]
            [ while cond body ] ]")
      (seps (vector #\Space
                    #\Newline
                    #\Tab)))
  (with-input-from-string (stream str)
    (loop with word = (read-next-word stream seps)
          until (null word)
          do
             (format t "~a~%" word)
             (setf word (read-next-word stream seps)))))
