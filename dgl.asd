(cl:in-package :cl-user)

(asdf:defsystem "dgl"
  :version "0.2.0"
  :description "OpenGL function loading from the spec."
  :author "Branan Landau"
  :license "zlib"
  :mailto "blandau@posteo.net"
  :depends-on ("cffi" "xdoc")
  :serial t
  :components ((:file "package")
	       (:static-file "gl.xml")
	       (:file "dgl"))
  :in-order-to ((asdf:test-op (asdf:test-op "dgl/test"))))

(asdf:defsystem "dgl/test"
  :version "0.0.1"
  :depends-on ("cl-glfw3" "transform3" "dgl")
  :pathname "test"
  :serial t
  :components ((:file "test")
	       (:file "triangle")
	       (:file "cube"))
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call "DGL-TEST" "RUN-TESTS")))
