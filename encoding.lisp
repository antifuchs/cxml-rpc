(in-package :cxml-rpc)


;; for debugging:
(defun encoded-request (function-name &rest args)
  (with-output-to-string (s)
    (funcall (apply #'encoder function-name args) s)))

;;; encoding

(defun encode-param (object)
  (with-element "param"
    (encode-value object)))

(defun encode-value (object)
  (with-element "value"
    (encode-object object)))

(defun escape-string (string)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (with-output-to-string (s)
    (loop for previous-posn = 0 then (1+ next-escapable-posn)
          for next-escapable-posn = (position-if (lambda (c)
                                                   (member c '(#\< #\> #\&)))
                                             string
                                             :start previous-posn)
          do (write-string string s :start previous-posn
                   :end (or next-escapable-posn (length string)))
          while next-escapable-posn
          do (ecase (char string next-escapable-posn)
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (#\& (write-string "&amp;" s))))))

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

(defgeneric encode-object (object)
  (:method ((o symbol))
    (with-element "boolean"
      (text
       (etypecase o
         (boolean (if o "1" "0"))))))
  (:method ((o integer))
    (with-element (etypecase o
                    ((signed-byte 32) "i4"))
      (text (format nil "~D" o))))
  (:method ((o float))
    (with-element "double"
      (text (format nil "~F" o))))
  (:method ((o string))
    (with-element "string"
      (text (escape-string o))))
  (:method ((o sequence))
    (with-element "array"
      (with-element "data"
        (map 'nil #'encode-value o))))
  (:method ((o file-stream))
    ;; TODO: um, handle things other than FILE-STREAMs
    (with-element "base64"
      (text
       (let* ((length (file-length o))
              (vector (make-array length
                                       :element-type (stream-element-type o))))
         (read-sequence vector o)
         (funcall (if (character-stream-p o)
                      #'cl-base64:string-to-base64-string
                      #'cl-base64:usb8-array-to-base64-string) vector)))))
  (:method ((o xml-rpc-date))
    (with-element "dateTime.iso8601"
      (text (iso8601-of o))))
  (:method ((o xrpc-struct))
    (with-element "struct"
      (dolist (member (member-names-of o))
        (with-element "member"
          (with-element "name"
            (text member))
          (encode-value (member-value member o)))))))