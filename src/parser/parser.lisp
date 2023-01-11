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
          (read-char stream nil 'the-end))
        (is-quoting nil))

    (loop until (word-readed-p separators c buffer is-quoting)
          do
             (if (char= c #\")
                 (setf is-quoting (not is-quoting)))
             (when (store-char-p separators c is-quoting)
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

(defun word-readed-p (separators c buffer is-quoting)
  (if (eql c 'the-end)
      t
      (and
       (not is-quoting)
       (separatorp separators c)
       (> (length buffer) 0))))

(defun store-char-p (separators c is-quoting)
  (if is-quoting
      t
      (not (separatorp separators c))))
