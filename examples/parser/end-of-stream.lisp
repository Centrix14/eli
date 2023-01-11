(with-open-file (stream "separatorp.lisp"
                        :direction :input
                        :if-does-not-exist :error)
  (loop with c until (end-of-stream-p stream) do
    (setf c (read-char stream))
    (write-char c)))
