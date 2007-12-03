(in-package #:cxml-rpc)

(defun report-error (fault-code fault-text)
  (values t (xrpc-struct "faultCode" fault-code "faultString" fault-text)))

(defvar *current-tag*)

(defun cxml-rpc-method-handler (tag)
  (labels
      ((dispatch ()
         (unless (eql (hunchentoot:request-method) :post)
           (error "Method must be POST"))
         (let ((stream (hunchentoot:raw-post-data :force-binary t))
               (*current-tag* tag))
           (multiple-value-call #'encode-response
             t
             (handler-case
                 (multiple-value-bind (method-name params)
                     (decode-method-call stream)
                   (setf (hunchentoot:content-type) "text/xml")
                   (multiple-value-call #'values
                     nil (apply (lookup-method tag method-name) params)))
               ;; error codes from
               ;; http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
               (method-not-found (c)
                 (report-error -32601 (princ-to-string c)))
               (program-error (c)
                 (report-error -32603 (princ-to-string c)))
               (klacks::klacks-error (c)
                 (report-error -32700
                               (format nil "Parse error from klacks: ~A" c)))
               (error (c)
                 (report-error -32500 (princ-to-string c))))))))
    #'dispatch))

(defparameter *system-methods*
              '(("system.listMethods" . list-methods)
                ("system.methodSignature" . method-signature)
                ("system.methodHelp" . method-help)
                ("system.getCapabilities" . system-capabilities)))

(defun lookup-method (tag method-name)
  ;; TODO: lookup user methods, throw method-not-found error if unfound.
  (or (cdr (assoc method-name *system-methods* :test #'equal))
      (lambda () (values (string tag) method-name))))

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

;;; introspection per <URL:http://xmlrpc-c.sourceforge.net/introspection.html>:

(defun list-methods ()
  "Lists the methods that are defined on this server."
  ;; TODO: list non-system methods
  (mapcar #'car *system-methods*))

(defun method-signature (method-name)
  "Returns an array of method signatures (arrays of strings listing first the 
return type, then the argument types)."
  ;; TODO: user-def methods.
  (case (lookup-method *current-tag* method-name)
    (list-methods '(("array")))
    (method-help '(("string" "string")))
    (method-signature '(("array" "string")))
    (system-capabilities '(("struct")))))

(defun method-help (method-name)
  "Returns a method's documentation as an XML-RPC string."
  (or (documentation (lookup-method *current-tag* method-name) 'function)
      "undef"))

;;; capabilities as per
;;; <URL:http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php>:

(defun system-capabilities ()
  "Returns a structural representation of the capabilities that this
xml-rpc implementation supports."
  (xrpc-struct "xmlrpc"
               (xrpc-struct "specUrl" "http://www.xmlrpc.com/spec"
                            "specVersion" "6/30/03 DW")
               "introspect"
               (xrpc-struct "specUrl"
                            "http://xmlrpc-c.sourceforge.net/introspection.html"
                            "specVersion" "1")
               "faults_interop"
               (xrpc-struct
                "specUrl"
                "http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php"
                "specVersion" "20010516")))