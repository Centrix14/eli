(defpackage :eli-parser
  (:use :common-lisp)
  (:export :parser
           :parser-buffer-word
           :parser-buffer-expression
           :parser-buffer-delimiter

           :eval-nestings))

(defclass parser ()
  ;; buffers
  ((buffer-word :reader parser-buffer-word
                :initarg :buffer-word
                :initform (make-array 0
                                      :fill-pointer 0
                                      :adjustable t
                                      :element-type 'character)
                :type string
                :documentation "The word buffer")
   (buffer-expression :reader parser-buffer-expression
                      :initarg :buffer-expression
                      :initform (make-array 0
                                            :fill-pointer 0
                                            :adjustable t
                                            :element-type 'string)
                      :type vector
                      :documentation "The expression buffer")
   (buffer-delimiter :reader parser-buffer-delimiter
                     :initarg :buffer-delimiter
                     :initform " "
                     :type string
                     :documentation "The buffer of delimiters")

   ;; nesting
   (nesting-degree :reader parser-nesting-degree
                   :initarg :nesting-degree
                   :initform 0
                   :type integer
                   :documentation "Indicates nesting degree of current word")
   (nesting-booster :reader parser-nesting-booster
                    :initarg :nesting-booster
                    :initform "["
                    :type string
                    :documentation "Contains word, that increments nesting")
   (nesting-reducer :reader parser-nesting-reducer
                    :initarg :nesting-reducer
                    :initform "]"
                    :type string
                    :documentation "Contains word, that decrements nesting"))

  (:documentation "The parser class"))

(defmethod print-object ((parser-object parser) stream)
  (format stream "~a ~a ~a ~a ~a ~a"
          (parser-buffer-word parser-object)
          (parser-buffer-expression parser-object)
          (parser-buffer-delimiter parser-object)
          (parser-nesting-degree parser-object)
          (parser-nesting-booster parser-object)
          (parser-nesting-reducer parser-object)))

(defmethod pars-char ((parser-object parser) char)
  (with-slots (buffer-word buffer-expression buffer-delimiter) parser-object
    (if (delimiterp parser-object char)
        (pars-char-handle-delimiter parser-object)
        (pars-char-handle-regular parser-object char))))

(defun pars-char-handle-delimiter (parser-object)
  (multiple-value-bind (current-nesting-degree updated-nesting-degree)
      (eval-nestings parser-object)
    (make-instance 'parser
                   :buffer-word (make-array 0
                                            :fill-pointer 0
                                            :adjustable t
                                            :element-type 'character)
                   :buffer-expression (concatenate
                                       'vector
                                       (parser-buffer-expression parser-object)
                                       (vector
                                        (list current-nesting-degree
                                              (parser-buffer-word parser-object))))
                   :buffer-delimiter (parser-buffer-delimiter parser-object)

                   :nesting-degree updated-nesting-degree
                   :nesting-booster (parser-nesting-booster parser-object)
                   :nesting-reducer (parser-nesting-reducer parser-object))))

(defun pars-char-handle-regular (parser-object char)
  (make-instance 'parser
                 :buffer-word (concatenate 'string
                                           (slot-value parser-object 'buffer-word)
                                           (string char))
                 :buffer-expression (parser-buffer-expression parser-object)
                 :buffer-delimiter (parser-buffer-delimiter parser-object)

                 :nesting-degree (parser-nesting-degree parser-object)
                 :nesting-booster (parser-nesting-booster parser-object)
                 :nesting-reducer (parser-nesting-reducer parser-object)))

(defmethod pars-stream ((parser-object parser) stream)
  (loop with c = (read-char stream nil 'the-end)
        while (not (eql c 'the-end)) do
          (setf parser-object (pars-char parser-object c))
          (setf c (read-char stream nil 'the-end)))
  (pars-char parser-object #\Space))

(defun delimiterp (parser-object char)
  (with-slots (buffer-delimiter) parser-object
    (if (position char buffer-delimiter)
        t
        nil)))
