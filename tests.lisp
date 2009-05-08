(cl:in-package :cxml-rpc)

(fiveam:def-suite :cxml-rpc)
(fiveam:in-suite :cxml-rpc)

(fiveam:test decode-response/double
  (fiveam:is
   (eql 1.06
        (decode-response "<methodResponse><params><param><value><double>1.06</double></value></param></params></methodResponse>"))))