(cl:in-package #:cxml-rpc)

(defun character-stream-p (stream)
  (subtypep (stream-element-type stream) 'character))

(defun call (uri function parameters &rest drakma-args)
    (apply #'call-with-encoder uri (apply 'encoder function parameters)
         #'decode-response
         drakma-args))

(defun trace-xml-response (output-stream uri function parameters
                           &rest drakma-args)
  (apply #'call-with-encoder uri (apply 'encoder function parameters)
         (lambda (stream)
           (let ((buffer
                  (make-array 1024 :element-type (stream-element-type stream))))
             (loop for nchars = (read-sequence buffer stream)
                   do (write-sequence buffer output-stream :end nchars)
                   while (= nchars (length buffer)))))
         drakma-args))

(defun call-with-encoder (uri encoder decoder &rest drakma-args
                          &key protocol cookie-jar basic-authorization
                          user-agent proxy proxy-basic-authentication
                          additional-headers redirect read-timeout
                          write-timeout connect-timeout)
  ;; need the keyword args for the invocation protocol only, so...
  (declare (ignore protocol cookie-jar basic-authorization
                   user-agent proxy proxy-basic-authentication
                   additional-headers redirect read-timeout
                   write-timeout connect-timeout))
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
           (funcall decoder stream))
          (t
           (error 'http-error :status-code status-code :reason reason-phrase
                  :headers headers)))
      (close stream))))

(defun encoder (function-name &rest args)
  (labels ((cxml-encoder (stream)
             (when (typep stream 'flexi-streams:flexi-stream)
              (setf (flexi-streams:flexi-stream-element-type stream) 'flexi-streams:octet))
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
                 (with-element "params"
                   (do-tagged-sequence (type value args)
                     (encode-param type value)))))))
    #'cxml-encoder))
