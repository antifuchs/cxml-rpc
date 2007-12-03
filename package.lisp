(cl:defpackage #:cxml-rpc
  (:use #:cl #:cxml)
  (:nicknames #:xml-rpc #:xrpc)
  (:export #:encoder #:call #:call-with-encoder
           #:encode-time #:decode-time
           ;; server:
           #:cxml-rpc-method-handler #:define-xrpc-method
           #:invoke-method #:lookup-method #:lookup-method-signature
           #:ensure-xrpc-method
           ;; conditions:
           #:cxml-rpc-condition #:network-error #:http-error
           #:cxml-rpc-fault #:bad-response-structure
           #:malformed-value-content
           ;; condition accessors:
           #:fault-code-of #:fault-phrase-of
           #:status-code-of #:reason-of #:headers-of
           #:faulty-element #:expected-element
           #:type-of-malformed-content #:malformed-content))