(with-open-file (stream "examples/ln/if.ln"
                        :direction :input
                        :if-does-not-exist :error)
  (format t "~%~%ln-eval-stream test~%")
  (ln-eval-stream stream))
