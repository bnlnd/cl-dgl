(cl:in-package :cl-user)

(defpackage :dgl-test/cube
  (:use :cl :cffi)
  (:export cube))

(in-package :dgl-test/cube)

; 6-------7
; |\      |\
; | 2-------3
; 4-|-----5 |
;  \|      \|
;   0-------1
;        6------7
;        |      |
;        |      |
; 6------2------3------7------6
; |      |      |      |      |
; |      |      |      |      |
; 4------0------1------5------4
;        |      |
;        |      |
;        4------5

(defparameter *cube-vertices*
  #2A((-0.5 -0.5 0.5)
      (0.5 -0.5 0.5)
      (-0.5 0.5 0.5)
      (0.5 0.5 0.5)
      (-0.5 -0.5 -0.5)
      (0.5 -0.5 -0.5)
      (-0.5 0.5 -0.5)
      (0.5 0.5 -0.5)))

(defparameter *tex-coords*
  #2A((0.0 0.0)
      (1.0 0.0)
      (1.0 1.0)
      (1.0 1.0)
      (0.0 1.0)
      (0.0 0.0)))

(defparameter *cube-indices*
  #2A((2 3 7) (7 6 2)
      (4 0 2) (2 6 4)
      (0 1 3) (3 2 0)
      (1 5 7) (7 3 1)
      (5 4 6) (6 7 5)
      (4 5 1) (1 0 4)))

(defparameter *cube-data*
  (let ((buf (make-array (list (* 6 6) (+ 3 2)) :element-type 'single-float)))
    (dotimes (i 12)
      (dotimes (j 3)
	(dotimes (k 3)
	  (setf (aref buf (+ j (* 3 i)) k) (aref *cube-vertices*
						 (aref *cube-indices* i j) k)))))
    (dotimes (i 36 buf)
      (dotimes (j 2)
	(setf (aref buf i (+ 3 j)) (aref *tex-coords* (mod i 6) j))))))

(defparameter *cube-texture*
  #2A((255 0 255 0)
      (0 255 0 255)
      (255 0 255 0)
      (0 255 0 255)))

(defparameter *vertex-source* "
#version 460 core

in vec3 vertex;
in vec2 texture_coords;
out vec2 texc;
uniform mat4 object;
uniform mat4 view;
uniform mat4 projection;

void main()
{
    texc = vec2(texture_coords.x, texture_coords.y);
    gl_Position = projection * view * object * vec4(vertex, 1.0);
}")

;; uniform samplerCube cube;
(defparameter *fragment-source* "
#version 460 core

in vec2 texc;
out vec4 color;
uniform sampler2D face;

void main()
{
    color = vec4(texture(face, texc).rrr, 1.0);
}")

(defun buffer-copy (ptr type array)
  (dotimes (i (array-total-size array))
    (setf (mem-aref ptr type i)
	  (row-major-aref array i))))

(defun copy-matrix (ptr mat4)
  (dotimes (i 4)
    (dotimes (j 4)
      (setf (mem-aref ptr 'GL:float (+ i (* j 4)))
	    (coerce (aref mat4 i j) 'single-float)))))

(defun vsize (array type)
  (* (array-total-size array)
     (foreign-type-size type)))

(glfw:def-window-size-callback viewport-callback (window w h)
  (declare (ignore window))
  (gl:Viewport 0 0 w h))

(defun make-cube-buffers (program arrptr bufptr texptr)
  (gl:UseProgram program)
  (with-foreign-objects ((vdata 'GL:float (array-total-size *cube-data*))
			 (texdata 'GL:ubyte (array-total-size *cube-texture*)))
    
    (buffer-copy vdata 'GL:float *cube-data*)
    (buffer-copy texdata 'GL:ubyte *cube-texture*)
    
    (gl:GenVertexArrays 1 arrptr)
    (gl:GenBuffers 1 bufptr)
    (gl:GenTextures 1 texptr)
    
    (let ((array (mem-ref arrptr 'GL:int))
	  (buffer (mem-ref bufptr 'GL:int))
	  (texture (mem-ref texptr 'GL:int))
	  (bufsize (vsize *cube-data* 'GL:float))
	  (vert-in (gl:GetProgramResourceLocation program GL:PROGRAM_INPUT "vertex"))
	  (tex-in (gl:GetProgramResourceLocation program GL:PROGRAM_INPUT "texture_coords"))
	  (sampler (gl:GetProgramResourceLocation program GL:UNIFORM "face"))
	  (stride (* 5 (foreign-type-size :float)))
	  (offset (* 3 (foreign-type-size :float))))

      (gl:ActiveTexture GL:TEXTURE0)
      (gl:BindTexture GL:TEXTURE_2D texture)
      (gl:TexParameteri GL:TEXTURE_2D GL:TEXTURE_WRAP_S GL:CLAMP_TO_BORDER)
      (gl:TexParameteri GL:TEXTURE_2D GL:TEXTURE_WRAP_T GL:CLAMP_TO_BORDER)
      (gl:TexParameteri GL:TEXTURE_2D GL:TEXTURE_MIN_FILTER GL:NEAREST_MIPMAP_NEAREST)
      (gl:TexParameteri GL:TEXTURE_2D GL:TEXTURE_MAG_FILTER GL:NEAREST)
      
      (gl:TexImage2D GL:TEXTURE_2D 0 GL:R8
		     (array-dimension *cube-texture* 0)
		     (array-dimension *cube-texture* 1)
		     0 GL:RED GL:UNSIGNED_BYTE texdata)
      (gl:GenerateMipmap GL:TEXTURE_2D)

      (gl:BindVertexArray array)
      (gl:BindBuffer GL:ARRAY_BUFFER buffer)
      (gl:BufferData GL:ARRAY_BUFFER bufsize vdata GL:STATIC_DRAW)
      (gl:VertexAttribPointer vert-in 3 GL:FLOAT GL:FALSE stride (make-pointer 0))
      (gl:EnableVertexAttribArray vert-in)
      (gl:VertexAttribPointer tex-in 2 GL:FLOAT GL:FALSE stride (make-pointer offset))
      (gl:EnableVertexAttribArray tex-in)

      (gl:Uniform1i sampler 0))))

(defun uniform (name program)
  (gl:GetProgramResourceLocation program GL:UNIFORM name))

(defun draw-cube (program array buffer texture angle matrix)
  (gl:UseProgram program)

  (let* ((size (glfw:get-window-size))
	 
	 (object-space (xf3:rotate-y (* 1.0d0 angle)))
	 
	 (view-space (xf3:look-at #(2.0d0 1.0d0 2.0d0)
				  #(0.0d0 0.0d0 0.0d0)
				  #(0.0d0 1.0d0 0.0d0)))

	 (projection-space (xf3:perspective (/ pi 2) (apply #'/ size) 0.01d0 100.0d0))
	 
	 (object (uniform "object" program))
	 (view (uniform "view" program))
	 (projection (uniform "projection" program)))
    
    (copy-matrix matrix object-space)
    (gl:UniformMatrix4fv object 1 GL:FALSE matrix)

    (copy-matrix matrix view-space)
    (gl:UniformMatrix4fv view 1 GL:FALSE matrix)

    (copy-matrix matrix projection-space)
    (gl:UniformMatrix4fv projection 1 GL:FALSE matrix))

  (gl:ActiveTexture GL:TEXTURE0)
  (gl:BindTexture GL:TEXTURE_2D texture)
  (gl:BindVertexArray array)
  (gl:BindBuffer GL:ARRAY_BUFFER buffer)
  (gl:Clear (logior GL:COLOR_BUFFER_BIT GL:DEPTH_BUFFER_BIT))
  (gl:DrawArrays GL:TRIANGLES 0 (array-dimension *cube-data* 0)))

(defun cube ()
  (setf dgl:*address-function* #'glfw:get-proc-address)
  (glfw:with-init-window (:title "dgl cube" :width 600 :height 400
			  :context-version-major 4
			  :context-version-minor 6
			  :opengl-profile :opengl-core-profile)

    (glfw:show-window)
    (glfw:make-context-current glfw:*window*)
    (glfw:swap-interval 1)
    (glfw:set-window-size-callback 'viewport-callback)
    
    (gl:Enable GL:DEBUG_OUTPUT)
    (gl:DebugMessageCallback (callback dgl:debug-callback) (make-pointer 0))
    
    (let* ((program (dgl-test/triangle:make-shader-program
		     (list (list GL:VERTEX_SHADER *vertex-source*)
			   (list GL:FRAGMENT_SHADER *fragment-source*)))))

      (gl:Enable GL:DEPTH_TEST)
      (gl:DepthFunc GL:LESS)
      (gl:Viewport 0 0 600 400)
      (gl:ClearColor 0.0 0.3 0.3 1.0)

      (with-foreign-objects ((arrptr 'GL:int)
			     (bufptr 'GL:int)
			     (texptr 'GL:int)
			     (matrix 'GL:float 16))
	(make-cube-buffers program arrptr bufptr texptr)
	
	(do ((array (mem-ref arrptr 'GL:int))
	     (buffer (mem-ref bufptr 'GL:int))
	     (texture (mem-ref texptr 'GL:int))
	     (start (get-internal-real-time))
	     (angle 0.0 (+ 0.01 angle)))
	    ((or (glfw:window-should-close-p)
		 (> (- (get-internal-real-time) start)
		    (* 3 internal-time-units-per-second))))
	  (sleep 0.01)
	  
	  (draw-cube program array buffer texture angle matrix)
	  
	  (glfw:swap-buffers)
	  (glfw:poll-events))
	
	(gl:DisableVertexAttribArray 0)
	(gl:BindVertexArray 0)
	(gl:DeleteVertexArrays 1 arrptr)
	(gl:DeleteBuffers 2 bufptr)
	(gl:DeleteProgram program)
	t))))

(pushnew 'cube dgl-test:*tests*)
