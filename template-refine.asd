;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:template-refine-asd
  (:use :cl :asdf))

(in-package #:template-refine-asd)

(defsystem template-refine
  :name "template-refine"
  :depends-on (:iterate :cxml :closure-html :cxml-stp :xpath :fiveam)
  :components ((:file "package")
               (:file "template-refine" :depends-on ("package"))
               (:file "tests" :depends-on ("template-refine"))))
