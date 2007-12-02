(in-package :cxml-rpc)


;;; xml-rpc's "ISO-8601" implementation

(defparameter *print-timestamps-in-utc-p* nil
  "Specifies whether so-called iso8601 timestamps are printed in the local time
zone (as mandated by the xml-rpc specification) or as UTC, which will probably 
break clients and servers.")

(defclass xml-rpc-date ()
     ((universal-time :initarg :universal-time :accessor universal-time-of)
      (iso8601 :initarg :iso8601 :accessor iso8601-of)))

;;; Structs

(defclass xrpc-struct ()
     ((value-map :initform (make-hash-table :test #'equal))))

(defun xrpc-struct (&rest key-value-pairs)
  (let ((struct (make-instance 'xrpc-struct)))
    (loop for (key value . rest) on key-value-pairs by #'cddr
          do (setf (member-value key struct) value))
    struct))

(defun member-names-of (struct)
  (loop for key being the hash-key in (slot-value struct 'value-map)
        collect key))

(defun member-value (name struct &optional default)
  (values (gethash name (slot-value struct 'value-map) default)))

(defun (setf member-value) (new-value name struct)
  (setf (gethash name (slot-value struct 'value-map)) new-value))

(defmethod print-object ((o xrpc-struct) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~{~S~^,~}" (member-names-of o))))