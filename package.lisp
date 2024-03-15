(cl:in-package :cl-user)

(defpackage :dgl
  (:use :cl :cffi)
  (:export #:*address-function*
	   #:*spec-path*
	   #:make-gl))
