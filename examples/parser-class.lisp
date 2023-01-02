(let ((my-parser (make-instance 'parser
                                :buffer-word "Just-an-initial-word"
                                :buffer-expression #((0 "First")
                                                     (0 "Second")
                                                     (0 "Third"))
                                :buffer-delimiter (vector #\Space
                                                          #\Tab
                                                          #\Newline)
                                :nesting-degree 0
                                :nesting-booster "("
                                :nesting-reducer ")")))
  (format t "Parser class example~%~%~a ~a ~a ~a ~a ~a~%"
          (parser-buffer-word my-parser)
          (parser-buffer-expression my-parser)
          (parser-buffer-delimiter my-parser)
          (parser-nesting-degree my-parser)
          (parser-nesting-booster my-parser)
          (parser-nesting-reducer my-parser)))
