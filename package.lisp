(cl:in-package :cl-user)

(defpackage :dgl
  (:use :cl :cffi)
  (:export #:*spec-url*
	   #:*address-function*
	   #:*spec-path*
	   #:download
	   #:make-gl

	   #:debug-callback))
