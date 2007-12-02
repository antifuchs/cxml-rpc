(in-package #:cxml-rpc)

(defun cxml-rpc-method-handler (tag)
  (labels
      ((dispatch ()
         (unless (eql (hunchentoot:request-method) :post)
           (error "Method must be POST"))
         (let ((stream (hunchentoot:raw-post-data :force-binary t)))
           (multiple-value-bind (method-name params) (decode-method-call stream)
             (setf (hunchentoot:content-type) "text/xml")
             (multiple-value-call #'encode-response
               t (handler-case
                     (multiple-value-call #'values
                       nil (apply (lookup-method tag method-name) params))
                   (error (c)
                     (values t
                             (xrpc-struct "faultCode" 666
                                          "faultString"
                                          (princ-to-string c))))))))))
    #'dispatch))

(defun lookup-method (tag method-name)
  ;; TODO
  (lambda () (values (string tag) method-name)))

(defun encode-response (octetp faultp &rest objects)
  (cxml:with-xml-output (funcall (if octetp
                                     #'cxml:make-octet-vector-sink
                                     #'cxml:make-string-sink)
                                 :indentation nil :canonical nil)
    (with-element "methodResponse"
      (if faultp
          (with-element "fault"
            (encode-value (first objects)))
          (with-element "params"
            (mapc #'encode-param objects))))))