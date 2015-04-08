;;;; package.lisp

(defpackage #:nebula
  (:use #:cl)
  (:export :set-store-path
	   :retrieve
	   :store
	   :info
	   :proxy
	   :lineage
	   :expunge
	   :proxy-all
	   :initialize))

