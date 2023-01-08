(with-open-file (stream "../ln/if.ln"
                        :direction :input
                        :if-does-not-exist :error)
  (ln-eval-stream stream))
