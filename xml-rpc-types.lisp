(in-package :cxml-rpc)

(defparameter *print-timestamps-in-utc-p* nil
  "Specifies whether so-called iso8601 timestamps are printed in the local time
zone (as mandated by the xml-rpc specification) or as UTC, which will probably 
break clients and servers.")

(defclass xml-rpc-date ()
     ((universal-time :initarg :universal-time :accessor universal-time-of)
      (iso8601 :initarg :iso8601 :accessor iso8601-of)))

(defun universal-time-to-xml-rpc-time-string (utime utc-p)
  (multiple-value-bind (second minute hour date month year day)
      (apply #'decode-universal-time utime (when utc-p (list 0)))
    (declare (ignore day))
    (format nil "~d~2,'0d~2,'0d~A~2,'0d:~2,'0d:~2,'0d"
            year month date
            (if utc-p #\Z #\T)
            hour minute second)))

(defun encode-time (utime &key (utc-p *print-timestamps-in-utc-p*))
  (make-instance 'xml-rpc-date
     :universal-time utime
     :iso8601 (universal-time-to-xml-rpc-time-string utime utc-p)))