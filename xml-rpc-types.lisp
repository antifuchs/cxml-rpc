(in-package :cxml-rpc)


;;; xml-rpc's "ISO-8601" implementation

(defparameter *print-timestamps-in-utc-p* nil
  "Specifies whether so-called iso8601 timestamps are printed in the local time
zone (as mandated by the xml-rpc specification) or as UTC, which will probably 
break clients and servers.")

(defclass xml-rpc-date ()
     ((universal-time :initarg :universal-time :accessor universal-time-of)
      (iso8601 :initarg :iso8601 :accessor iso8601-of)))

(defmethod print-object ((o xml-rpc-date) s)
  (print-unreadable-object (o s :type t :identity nil)
    (write-string (iso8601-of o) s)))

