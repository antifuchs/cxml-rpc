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
    (declare (ignore body-or-stream must-close uri))
    (unwind-protect
        (case status-code
          (200
           (decode-response stream))
          (t
           (error 'http-error :status-code status-code :reason reason-phrase
                  :headers headers)))
      (close stream))))

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
