(cl:defpackage #:cxml-rpc.package
  (:use :cl :asdf))

(cl:in-package #:cxml-rpc.package)

(defsystem #:cxml-rpc
  :depends-on (#:cxml #:cl-base64 #:drakma)
  :components
  ((:file "package")
   (:file "conditions" :depends-on ("package"))
   (:file "xml-rpc-types" :depends-on ("conditions"))
   (:file "encode" :depends-on ("xml-rpc-types"))
   (:file "decode" :depends-on ("xml-rpc-types"))
   (:file "client" :depends-on ("encode" "decode"))))