(cl:defpackage #:cxml-rpc.package
  (:use :cl :asdf))

(cl:in-package #:cxml-rpc.package)

(defsystem #:cxml-rpc
  :description "An XML-RPC interface for Common Lisp"
  :depends-on (#:cxml #:cl-base64 #:drakma #:hunchentoot #:parse-number)
  :components
  ((:file "package")
   (:file "conditions" :depends-on ("package"))
   (:file "xml-rpc-types" :depends-on ("conditions"))
   (:file "encoding" :depends-on ("xml-rpc-types"))
   (:file "decoding" :depends-on ("xml-rpc-types"))
   (:file "client" :depends-on ("encoding" "decoding"))
   (:file "server" :depends-on ("encoding" "decoding"))))

(defsystem #:cxml-rpc-tests
  :depends-on (#:cxml #:fiveam)
  :components ((:file "tests")))