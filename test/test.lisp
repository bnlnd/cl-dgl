(cl:in-package :cl-user)

(defpackage :dgl-test
  (:use :cl :cffi)
  (:export run-tests
	   *tests*
	   debug-callback))

(in-package :dgl-test)

(defparameter *tests* nil)

(defcallback debug-callback :void
    ((source GL:enum)
     (type GL:enum)
     (id GL:uint)
     (severity GL:enum)
     (length GL:sizei)
     (message :string)
     (userParam :pointer))
  (declare (ignore id userParam length))
  (let ((clsource (ecase source
		    (#.GL:DEBUG_SOURCE_API :api)
		    (#.GL:DEBUG_SOURCE_WINDOW_SYSTEM :window-system)
		    (#.GL:DEBUG_SOURCE_SHADER_COMPILER :shader-compiler)
		    (#.GL:DEBUG_SOURCE_THIRD_PARTY :third-party)
		    (#.GL:DEBUG_SOURCE_APPLICATION :application)
		    (#.GL:DEBUG_SOURCE_OTHER :other)))
	(cltype (ecase type
		  (#.GL:DEBUG_TYPE_ERROR :error)
		  (#.GL:DEBUG_TYPE_DEPRECATED_BEHAVIOR :deprecated)
		  (#.GL:DEBUG_TYPE_UNDEFINED_BEHAVIOR :undefined)
		  (#.GL:DEBUG_TYPE_PORTABILITY :portability)
		  (#.GL:DEBUG_TYPE_PERFORMANCE :performance)
		  (#.GL:DEBUG_TYPE_MARKER :marker)
		  (#.GL:DEBUG_TYPE_PUSH_GROUP :push-group)
		  (#.GL:DEBUG_TYPE_POP_GROUP :pop-group)
		  (#.GL:DEBUG_TYPE_OTHER :other)))
	(clseverity (ecase severity
		      (#.GL:DEBUG_SEVERITY_HIGH :high)
		      (#.GL:DEBUG_SEVERITY_MEDIUM :medium)
		      (#.GL:DEBUG_SEVERITY_LOW :low)
		      (#.GL:DEBUG_SEVERITY_NOTIFICATION :notification))))
    (unless (eq :notification clseverity)
      (format t "msg ~A/~A/~A: ~A~%" clsource clseverity cltype message))))

(defun run-tests ()
  (dolist (test (reverse *tests*))
    (format t "~(~A~): " test)
    (format t "~A~%"
	    (handler-case (funcall test)
	      (error (e) (class-name (class-of e)))
	      (:no-error (v) (if v "ok" "fail"))))))
