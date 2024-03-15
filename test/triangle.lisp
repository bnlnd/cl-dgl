(cl:in-package :cl-user)

(defpackage :dgl-test/triangle
  (:use :cl :cffi)
  (:export #:triangle
	   #:compile-shader
	   #:make-shader-program))

(in-package :dgl-test/triangle)

(defparameter *vertices*
  #2A((0.0 0.5)
      (-0.5 -0.5)
      (0.5 -0.5)))

(defparameter *vertex-source* "
#version 460 core
layout (location=0) in vec2 vert;

void main()
{
  gl_Position = vec4(vert, 0.0, 1.0);
}")

(defparameter *fragment-source* "
#version 460 core
out vec4 color;

void main()
{
  color = vec4(1.0, 1.0, 1.0, 1.0);
}")

(glfw:def-window-size-callback viewport-callback (window w h)
  (declare (ignore window))
  (gl:Viewport 0 0 w h))

(defun compile-shader (type source)
  (let ((shader (gl:CreateShader type))
	(len 0))
    (with-foreign-objects ((iptr :int)
			   (cptr :pointer))
      (with-foreign-string (cstr source)
	(setf (mem-ref cptr :pointer) cstr)
	(gl:ShaderSource shader 1 cptr (null-pointer)))
      (gl:CompileShader shader)
      (gl:GetShaderiv shader GL:COMPILE_STATUS iptr)
      (when (zerop (mem-ref iptr :int))
	(gl:GetShaderiv shader GL:INFO_LOG_LENGTH iptr)
	(setf len (mem-ref iptr :int))
	(with-foreign-objects ((msg :char len))
	  (gl:GetShaderInfoLog shader len (null-pointer) msg)
	  (gl:DeleteShader shader)
	  (error "~A" (foreign-string-to-lisp msg)))))
    shader))

(defun make-shader-program (shaders)
  (let ((program (gl:CreateProgram))
	(compiled (mapcar (lambda (entry)
			    (apply #'compile-shader entry))
			  shaders))
	(len 0))
    (dolist (shader compiled)
      (gl:AttachShader program shader))
    (gl:LinkProgram program)
    (with-foreign-objects ((iptr :int))
      (gl:GetProgramiv program GL:LINK_STATUS iptr)
      (unwind-protect
	   (when (zerop (mem-ref iptr :int))
	     (gl:GetProgramiv program GL:INFO_LOG_LENGTH iptr)
	     (setf len (mem-ref iptr :int))
	     (with-foreign-objects ((msg :char len))
	       (gl:GetProgramInfoLog program len (null-pointer) msg)
	       (gl:DeleteProgram program)
	       (error "~A" (foreign-string-to-lisp msg))))
	(dolist (shader compiled)
	  (gl:DetachShader program shader)
	  (gl:DeleteShader shader))))
    program))

(defun make-vertex-data (aptr bptr)
  (let ((sizeof (* (array-total-size *vertices*) (foreign-type-size :float)))
	(single-size (* (array-dimension *vertices* 1) (foreign-type-size :float))))
    
    (gl:GenVertexArrays 1 aptr)
    (gl:GenBuffers 1 bptr)

    (let ((array (mem-ref aptr 'gl:uint))
	  (buffer (mem-ref bptr 'gl:uint)))

      
      (gl:BindBuffer GL:ARRAY_BUFFER buffer)
      (gl:BufferStorage GL:ARRAY_BUFFER sizeof (null-pointer) GL:MAP_WRITE_BIT)

      (let ((ptr (gl:MapNamedBuffer buffer GL:WRITE_ONLY)))
	(dotimes (i (array-total-size *vertices*))
	  (setf (mem-aref ptr :float i)
		(row-major-aref *vertices* i)))
	(gl:UnmapNamedBuffer buffer))


      (gl:BindVertexArray array)

      (gl:VertexAttribPointer 0 2 GL:FLOAT GL:FALSE single-size (null-pointer))
      (gl:EnableVertexAttribArray 0))))

(defun triangle ()

  (setf dgl:*address-function* #'glfw:get-proc-address)
  
  (glfw:with-init-window (:title "dgl triangle" :width 600 :height 400
			  :context-version-major 4
			  :context-version-minor 6
			  :opengl-profile :opengl-core-profile)

    (glfw:show-window)
    (glfw:make-context-current glfw:*window*)
    (glfw:set-window-size-callback 'viewport-callback)

    (gl:Enable GL:DEBUG_OUTPUT)
    (gl:DebugMessageCallback (callback dgl-test:debug-callback) (make-pointer 0))

    (let ((program (make-shader-program
		    (list (list GL:VERTEX_SHADER *vertex-source*)
			  (list GL:FRAGMENT_SHADER *fragment-source*)))))

      (gl:Viewport 0 0 600 400)
      (gl:ClearColor 0.0 0.3 0.3 1.0)

      (with-foreign-objects ((array :int)
			     (buffer :int))
	(make-vertex-data array buffer)
	
	(do ((arr (mem-ref array :int))
	     (start (get-internal-real-time)))
	    ((or (glfw:window-should-close-p)
		 (> (- (get-internal-real-time) start)
		    (* 3 internal-time-units-per-second))))
	  (sleep 0.01)
	  
	  (gl:Clear GL:COLOR_BUFFER_BIT)
	  (gl:UseProgram program)
	  (gl:BindVertexArray arr)
	  (gl:DrawArrays GL:TRIANGLES 0 3)
	  
	  (glfw:swap-buffers)
	  (glfw:poll-events))
	
	(gl:DisableVertexAttribArray 0)
	(gl:BindVertexArray 0)
	(gl:DeleteVertexArrays 1 array)
	(gl:DeleteBuffers 1 buffer)
	(gl:DeleteProgram program)
	t))))

(pushnew 'triangle dgl-test:*tests*)
