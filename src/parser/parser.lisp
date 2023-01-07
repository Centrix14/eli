(defun read-next-word (stream &optional (separators " "))
  (if (end-of-stream-p stream)
      nil
      (%read-next-word% stream separators)))

(defun %read-next-word% (stream separators)
  (let ((buffer (make-array 0
                            :fill-pointer 0
                            :adjustable t
                            :element-type 'character))
        (c
          (read-char stream nil 'the-end)))

    (loop until (or
                 (eql c 'the-end)
                 (and (separatorp separators c)
                      (> (length buffer) 0)))
          do
             (unless (separatorp separators c)
                 (vector-push-extend c buffer))

             (setf c (read-char stream nil 'the-end)))

    buffer))

(defun separatorp (separators char)
  (if (position char separators)
      t
      nil))

(defun end-of-stream-p (stream)
  (let ((c
          (read-char stream nil 'the-end))
        (result nil))

    (if (eql c 'the-end)
        (return-from end-of-stream-p t))

    (unread-char c stream)
    nil))
