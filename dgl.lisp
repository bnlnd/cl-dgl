(cl:in-package :dgl)

(defparameter *spec-path*
  (asdf:system-relative-pathname "dgl" "gl.xml"))

(xdoc:define-document *gl.xml*
    (:name "registry"
     :children ((:name "enums"
		 :children ((:name "enum"
			     :entry add-enum)))
		(:name "commands"
		 :children ((:name "command"
			     :whole add-command)))
		(:name "feature"
		 :children ((:name "remove"
			     :children ((:name "command"
					 :entry remove-command)
					(:name "enum"
					 :entry remove-enum))))))))

(defvar *enums*)
(defvar *funcs*)

(defvar *address-function*)

;; does not include tags with no core counterpart
(defparameter *vendors*
  '("EXT" "KHR" "OES" "NV" "APPLE" "ATI" "AMD" "PGI" "QCOM"
    "MESA" "INTEL" "SUN" "ARB" "IBM" "SGI" "SGIX" "SGIS" "PGI"))

(defun initialize-type-table ()
  (let ((types '(("void" :void)
		 ("GLvoid" :void)
		 ("GLboolean" :unsigned-char)
		 ("GLbitfield" :unsigned-int)
		 ("GLbyte" :int8)
		 ("GLubyte" :uint8)
		 ("GLshort" :int16)
		 ("GLushort" :uint16)
		 ("GLhalf" :uint16)
		 ("GLhalfARB" :uint16)
		 ("GLhalfNV" :short)
		 ("GLhalf16NV" :short)
		 ("GLint" :int)
		 ("GLuint" :unsigned-int)
		 ("GLenum" :unsigned-int)
		 ("GLclampx" :int32)
		 ("GLfixed" :int32)
		 ("GLsizei" :int)
		 ("GLsizeiptrARB" :int)
		 ("GLint64" :int64)
		 ("GLint64EXT" :int64)
		 ("GLuint64" :uint64)
		 ("GLuint64EXT" :uint64)
		 ("GLfloat" :float)
		 ("GLclampf" :float)
		 ("GLdouble" :double)
		 ("GLclampd" :double)
		 ("GLeglClientBufferEXT" :pointer)
		 ("GLeglImageEOS" :pointer)
		 ("GLDEBUGPROC" :pointer) 
		 ("GLVULKANPROCNV" :pointer)
		 ("GLvdpauSurfaceNV" :pointer)
		 ("GLsync" :pointer)
		 ("GLeglClientBufferEXT" :pointer)
		 ("GLeglImageOES" :pointer)
		 ("GLDEBUGPROC" :pointer)
		 ("GLchar" :char)
		 ("GLcharARB" :char)
		 ("GLhandleARB" #+:darwin :pointer #-:darwin :unsigned-int)
		 ("GLintptr" :pointer)
		 ("GLintptrARB" :pointer)
		 ("GLsizeiptr" :ssize)
		 ("GLsizeiptr" :ssize))))
    (dolist (type types)
      (destructuring-bind (name cffitype) type
	(eval `(cffi:defctype ,(make-gl-symbol (subseq name 2)) ,cffitype))))))

(defun make-gl-symbol (name)
  (let* ((pkg (or (find-package "GL")
		  (make-package "GL")))
	 (sym (intern (string-upcase name) pkg)))
    (export sym pkg)
    sym))

(defun parse-c-integer (fig)
  (let ((hexp (and (> (length fig) 2)
		   (string= (subseq fig 0 2) "0x"))))
    (parse-integer fig :start (if hexp 2 0)
		       :radix (if hexp 16 10))))

(defun count-in-tag (char tag)
  (reduce #'+ (mapcar (lambda (str) (count char str))
		      (remove-if-not #'stringp (xdoc:children tag)))))

(defun ends-with (suffix string)
  (string= suffix (subseq string (max 0 (- (length string) (length suffix))))))

(defun vendor-tagged-p (name)
  (some (lambda (vendor) (ends-with vendor name)) *vendors*))

(defun resolve-type (name)
  (if (stringp name)
      (make-gl-symbol (subseq name 2))
      name))

(defun tag-type (tag)
  (let ((preamble (xdoc:find-child :text tag))
	(ptype (and (xdoc:find-child "ptype" tag)
		    (xdoc:text "ptype" tag))))
    (cond ((null preamble) ptype)
	  ((search "const void *" preamble) :pointer)
	  ((search "void *" preamble) :pointer)
	  ((plusp (count-in-tag #\* tag))
	   (if (string= ptype "GLchar") :string :pointer))
	  ((search "void" preamble) :void)
	  (t (error "internal: did not handle type ~S" preamble)))))

(defun add-enum (tag his)
  (declare (ignore his))
  (let ((name (xdoc:attr "name" tag))
	(value (xdoc:attr "value" tag)))
    (unless (vendor-tagged-p name)
      (setf (gethash name *enums*)
	    (parse-c-integer value)))))

(defun add-command (tag his)
  (declare (ignore his))
  (let* ((proto (xdoc:find-child "proto" tag))
	 (name (xdoc:text "name" proto)))
    (unless (vendor-tagged-p name)
      (setf (gethash name *funcs*)
	    (list (tag-type proto)
		  (mapcar (lambda (child)
			    (list (xdoc:text "name" child)
				  (tag-type child)))
			  (remove "param" (xdoc:children tag)
				  :key #'xdoc:name
				  :test #'string/=)))))))

(defun remove-enum (tag his)
  (declare (ignore his))
  (remhash (xdoc:attr "name" tag) *enums*))

(defun remove-command (tag his)
  (declare (ignore his))
  (remhash (xdoc:attr "name" tag) *funcs*))

(defun make-enum (name value)
  (eval `(defconstant ,(make-gl-symbol (subseq name 3)) ,value)))

(defun make-command (name entry)
  (destructuring-bind (type args) entry
    (let* ((sym (make-gl-symbol (subseq name 2)))
	   (args* (mapcar #'make-symbol
			  (mapcar #'string-upcase
				  (mapcar #'first args))))
	   (c-args (mapcan #'list
			   (mapcar #'resolve-type
				   (mapcar #'second args))
			   args*)))
      (compile sym
	       `(lambda ,args*
		  (assert (not (null *address-function*)))
		  (let* ((address (funcall *address-function* ,name))
			 (func (lambda ,args*
				 (foreign-funcall-pointer address nil ,@c-args
							  ,(resolve-type type)))))
		    (unless address
		      (error "Cannot load function ~S" ,name))
		    (setf (fdefinition ',sym) func)
		    (funcall func ,@args*)))))))

(defun compile-spec (path)
  (let ((*enums* (make-hash-table :test 'equal))
	(*funcs* (make-hash-table :test 'equal)))
    (xdoc:mapdoc *gl.xml* path)
    (values *enums* *funcs*)))

(defun make-gl ()
  (multiple-value-bind (enums funcs) (compile-spec *spec-path*)
    (initialize-type-table)
    (maphash #'make-enum enums)
    (maphash #'make-command funcs)))

(make-gl)
