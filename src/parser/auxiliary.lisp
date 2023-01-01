(defun eval-nestings (parser-object)
  (with-slots (buffer-word nesting-degree
               nesting-booster nesting-reducer) parser-object
    (cond
      ((string= buffer-word nesting-booster)
       (values nesting-degree (1+ nesting-degree)))

      ((string= buffer-word nesting-reducer)
       (values (1- nesting-degree) (1- nesting-degree)))

      (t
       (values nesting-degree nesting-degree)))))
