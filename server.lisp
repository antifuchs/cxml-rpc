(in-package #:cxml-rpc)

(defun report-error (fault-code fault-text)
  (values t
          `(("faultCode" :integer ,fault-code)
            ("faultString" :string ,fault-text))
          :struct))

(defvar *current-tag*)

(defun cxml-rpc-method-handler (tag)
  (labels
      ((dispatch ()
         (unless (eql (hunchentoot:request-method) :post)
           (error "Method must be POST"))
         (let ((stream (hunchentoot:raw-post-data :force-binary t))
               (*current-tag* tag))
           (multiple-value-bind (method-name params)
               (decode-method-call stream)
             (setf (hunchentoot:content-type) "text/xml") 
             (invoke-method method-name params)))))
    #'dispatch))

(defparameter *system-methods*
              '(("system.listMethods" . system-list-methods)
                ("system.methodSignature" . system-method-signature)
                ("system.methodHelp" . system-method-help)
                ("system.getCapabilities" . system-capabilities)))

(defun lookup-method (tag method-name)
  ;; TODO: lookup user methods, throw method-not-found error if unfound.
  (or (cdr (assoc method-name *system-methods* :test #'equal))
      (lambda () (values (string tag) method-name))))

(defun invoke-method (method-name params
                      &key (octetp t) ((:tag *current-tag*) *current-tag*))
  (multiple-value-call #'encode-response
    octetp
    (handler-case
        (multiple-value-call #'values
          nil (apply (lookup-method *current-tag* method-name) params))
      ;; error codes from
      ;; http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
      (method-not-found (c)
        (report-error -32601 (princ-to-string c)))
      (program-error (c)
        (report-error -32603 (princ-to-string c)))
      (klacks::klacks-error (c)  ; doesn't seem to be defined?
               (report-error -32700
                             (format nil "Parse error from klacks: ~A" c)))
      (error (c)
        (report-error -32500 (princ-to-string c))))))

(defun encode-response (octetp faultp object &optional
                        (type (dwim-type-for object)))
  (cxml:with-xml-output (funcall (if octetp
                                     #'cxml:make-octet-vector-sink
                                     #'cxml:make-string-sink)
                                 :indentation nil :canonical nil)
    (with-element "methodResponse"
      (if faultp
          (with-element "fault"
            (encode-value type object))
          (with-element "params"
            (encode-param type object))))))

;;; introspection per <URL:http://xmlrpc-c.sourceforge.net/introspection.html>:

(defun system-list-methods ()
  "Lists the methods that are defined on this server."
  ;; TODO: list non-system methods
  (values (mapcar #'car *system-methods*)
          :dwim-array))

(defun system-method-signature (method-name)
  "Returns an array of method signatures (arrays of strings listing first the 
return type, then the argument types)."
  ;; TODO: user-def methods.
  (values
   (case (lookup-method *current-tag* method-name)
     (list-methods '(("array")))
     (method-help '(("string" "string")))
     (method-signature '(("array" "string")))
     (system-capabilities '(("struct"))))
   :dwim-array))

(defun system-method-help (method-name)
  "Returns a method's documentation as an XML-RPC string."
  (values  (or (documentation (lookup-method *current-tag* method-name)
                              'function)
               "undef")
           :string))

;;; capabilities as per
;;; <URL:http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php>:

(defun system-capabilities ()
  "Returns a structural representation of the capabilities that this
xml-rpc implementation supports."
  
  (values '((
             "xmlrpc"
             :struct (("specUrl" :string "http://www.xmlrpc.com/spec")
                      ("specVersion" :string "6/30/03 DW"))
             "introspect"
             :struct (("specUrl"
                       :string
                       "http://xmlrpc-c.sourceforge.net/introspection.html")
                      ("specVersion" :string "1"))
             "faults_interop"
             (("specUrl"
               :string
               "http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php")
              ("specVersion" :string "20010516"))))
          :struct))