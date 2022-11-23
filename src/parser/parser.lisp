(defclass parser ()
  ((buffer-word :reader parser-buffer-word
				:initarg :buffer-word
				:initform ""
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
   (:documentation "The parser class")))
