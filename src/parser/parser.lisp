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
					 :documentation "The buffer of delimiters")
   (need-new-nesting-layer :reader parser-need-new-nesting-layer
						   :initarg :need-new-nesting-layer
						   :initform nil
						   :type symbol
						   :documentation
						   "Indicates, that need to create new nesting layer"))
  (:documentation "The parser class"))

(defmethod pars-char ((parser-object parser) char)
  (with-slots (buffer-word buffer-expression buffer-delimiter) parser-object
	  (if (delimiterp parser-object char)
		  (pars-char-handle-delimiter parser-object)
		  (pars-char-handle-regular parser-object char))))

(defun pars-char-handle-delimiter (parser-object)
  (format t "word: ~a~%" (slot-value parser-object 'buffer-word))
  (make-instance 'parser
				 :buffer-word (make-array 0
										  :fill-pointer 0
										  :adjustable t
										  :element-type 'character)
				 :buffer-expression (parser-buffer-expression parser-object)
				 :buffer-delimiter (parser-buffer-delimiter parser-object)
				 :need-new-nesting-layer (parser-need-new-nesting-layer parser-object)))

(defun pars-char-handle-regular (parser-object char)
  (make-instance 'parser
				 :buffer-word (concatenate 'string
										   (slot-value parser-object 'buffer-word)
										   (string char))
				 :buffer-expression (parser-buffer-expression parser-object)
				 :buffer-delimiter (parser-buffer-delimiter parser-object)
				 :need-new-nesting-layer (parser-need-new-nesting-layer parser-object)))

(defun delimiterp (parser-object char)
  (with-slots (buffer-delimiter) parser-object
	(if (position char buffer-delimiter)
		t
		nil)))
