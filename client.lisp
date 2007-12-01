(cl:in-package #:cxml-rpc)

(defun character-stream-p (stream)
  (subtypep (stream-element-type stream) 'character))

(defun call (uri function parameters &rest drakma-args)
  (apply #'call-with-encoder uri (apply 'encoder function parameters)
         drakma-args))

(defun call-with-encoder (uri encoder &rest drakma-args
                          &key protocol cookie-jar basic-authorization
                          user-agent proxy proxy-basic-authentication
                          additional-headers redirect read-timeout
                          write-timeout)
  ;; need the keyword args for the invocation protocol only, so...
  (declare (ignore protocol cookie-jar basic-authorization
                   user-agent proxy proxy-basic-authentication
                   additional-headers redirect read-timeout
                   write-timeout))
  (multiple-value-bind (body-or-stream status-code headers uri stream must-close
                                       reason-phrase)
      (apply #'drakma:http-request uri
             :method :post
             :content-type "text/xml"
             :content-length t
             :content encoder
             :want-stream t
             drakma-args)
    (ecase status-code
      (200 (parse-stream stream (cxml-xmls:make-xmls-builder))))))

(defun encoder (function-name &rest args)
  (labels ((cxml-encoder (stream)
             (cxml:with-xml-output
                 (funcall (if (character-stream-p stream)
                              #'cxml:make-character-stream-sink
                              #'cxml:make-octet-stream-sink)
                          stream
                          :indentation nil
                          :canonical nil)
               (with-element "methodCall"
                 (with-element "methodName"
                   (text function-name))
                 (unless (null args)
                   (with-element "params"
                     (mapc #'encode-param args)))))))
    #'cxml-encoder))

;; for debugging:
(defun encoded-request (function-name &rest args)
  (with-output-to-string (s)
    (funcall (apply #'encoder function-name args) s)))

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
  ;; TODO: structs
  )