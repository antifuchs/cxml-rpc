(cl:in-package :cxml-rpc)

(fiveam:def-suite :cxml-rpc)
(fiveam:in-suite :cxml-rpc)

(fiveam:test decode-response/double
  (fiveam:is
   (equal (list 1.06 :double)
          (multiple-value-list
           (decode-response "<methodResponse><params><param><value><double>1.06</double></value></param></params></methodResponse>")))))