(cl:in-package #:cxml-rpc)

(define-condition cxml-rpc-fault (error) ())

(define-condition network-error (cxml-rpc-fault) ())
