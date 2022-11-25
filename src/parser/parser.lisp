(defpackage :eli-parser
  (:use :common-lisp)
  (:export :parser
		   :parser-buffer-word
		   :parser-buffer-expression
		   :parser-buffer-delimiter))

(defclass parser ()
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
					 :documentation "The buffer of delimiters"))
  (:documentation "The parser class"))

(defmethod pars-char ((parser-object parser) char)
  (with-slots (buffer-word buffer-expression buffer-delimiter) parser-object
	  (if (delimiterp parser-object char)
		  (pars-char-handle-delimiter parser-object)
		  (pars-char-handle-regular parser-object char)))
  parser-object)

(defun pars-char-handle-delimiter (parser-object)
  (with-slots (buffer-word) parser-object
	(format t "word: ~a~%" buffer-word)
	(setf (fill-pointer buffer-word) 0)))

(defun pars-char-handle-regular (parser-object char)
  (with-slots (buffer-word) parser-object
	(vector-push-extend char buffer-word)))

(defun delimiterp (parser-object char)
  (with-slots (buffer-delimiter) parser-object
	(if (position char buffer-delimiter)
		t
		nil)))
