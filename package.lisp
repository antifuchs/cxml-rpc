(cl:defpackage #:cxml-rpc
  (:use #:cl #:cxml)
  (:nicknames #:xml-rpc #:xrpc)
  (:export #:encoder #:call #:call-with-encoder
           #:encode-time #:decode-time
           #:xstruct #:xstruct-slot-value))