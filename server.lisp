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
           (multiple-value-bind (method-name param-types params)
               (decode-method-call stream)
             (setf (hunchentoot:content-type) "text/xml") 
             (invoke-method method-name param-types params)))))
    #'dispatch))

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

;;; capabilities as per
;;; <URL:http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php>:

(defun system-capabilities ()
  "Returns a structural representation of the capabilities that this
xml-rpc implementation supports."
  (values '((
             "xmlrpc"
             :struct (("specUrl" :string "http://www.xmlrpc.com/spec")
                      ("specVersion" :string "6/30/03 DW")))
            ("introspect"
             :struct (("specUrl"
                       :string
                       "http://xmlrpc-c.sourceforge.net/introspection.html")
                      ("specVersion" :string "1")))
            ("faults_interop"
             :struct
             (("specUrl"
               :string
               "http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php")
              ("specVersion" :string "20010516"))))
          :struct))


;;; user-defined methods

(defvar *user-defined-methods* (make-hash-table :test #'equalp)
  "Methods stored under the key (handler-tag . \"method-name\").")

(defvar *user-defined-method-signatures* (make-hash-table :test #'equalp)
  "Method signatures stored under the key (handler-tag . \"method-name\").")

(defun ensure-xrpc-method (handler-tag name signature documentation function)
  (setf (gethash (cons handler-tag name) *user-defined-methods*)
        function
        (gethash (cons handler-tag name) *user-defined-method-signatures*)
        signature)
  (setf (documentation function t) documentation)
  name)

(defparameter *system-methods*
              '(("system.listMethods" system-list-methods
                 (:array))
                ("system.methodSignature" system-method-signature
                 (:struct :string))
                ("system.methodHelp" system-method-help
                 (:string :string))
                ("system.getCapabilities" system-capabilities
                 (:struct))))

(defun lookup-method (handler-tag method-name)
  (or (second (assoc method-name *system-methods* :test #'equal))
      (gethash (cons handler-tag method-name) *user-defined-methods*)
      (error 'method-not-found :name method-name :tag handler-tag)))

(defun fix-method-signature (signature)
  (append (butlast signature) (cons (car (last signature)) nil)))

(defun lookup-method-signature (handler-tag method-name &key fixup-p)
  (or (third (assoc method-name *system-methods* :test #'equal))
      (funcall (if fixup-p
                   #'fix-method-signature
                   #'identity)
       (gethash (cons handler-tag method-name)
                *user-defined-method-signatures*))
      (error 'method-not-found :name method-name :tag handler-tag)))

(defun signature-arg-count (signature)
  (loop for (elt . rest) on signature
        count elt into result
        if (numberp rest)
          do (return (values result (+ rest result)))
        else if (eql rest t)
               do (return (values result nil))
        finally (return (values result result))))

(defun check-method-signature-for-invocation (param-types signature/return
                                              &aux (signature
                                                    (cdr signature/return)))
  (multiple-value-bind (minimum maximum) (signature-arg-count signature)
    (unless (<= minimum (length param-types)
                (or maximum (1+ (max minimum (length param-types)))))
      (error 'method-arg-count-mismatch
             :expected-max (or maximum "infinite") :expected-min minimum
             :got (length param-types))))
  (loop for (param-type . rem-params) on param-types
        for (sig-type . rem-signature) on signature
        for position from 0
        unless (eql param-type sig-type)
          do (error 'method-arg-type-mismatch
                    :position position :expected sig-type :got param-type)))

(defun invoke-method (method-name param-types params
                      &key (octetp t) ((:tag *current-tag*) *current-tag*))
  (multiple-value-call #'encode-response
    octetp
    (handler-case
        (progn
          (check-method-signature-for-invocation
           param-types (lookup-method-signature *current-tag* method-name))
          (multiple-value-call #'values
            nil (apply (lookup-method *current-tag* method-name) params)))
      ;; error codes from
      ;; http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
      (method-not-found (c)
        (report-error -32601 (princ-to-string c)))
      (method-arg-type-mismatch (c)
        (report-error -32602 (princ-to-string c)))
      (method-arg-count-mismatch (c)
        (report-error -32602 (princ-to-string c)))
      (program-error (c)
        (report-error -32603 (princ-to-string c)))
      (klacks::klacks-error (c)         ; doesn't seem to be defined?
        (report-error -32700
                      (format nil "Parse error from klacks: ~A" c)))
      (error (c)
        (report-error -32500 (princ-to-string c))))))

(defmacro define-xrpc-method ((name handler-tag) (&rest args) (&rest signature)
                              &body optional-docstring-and-body)
  (let* ((docstring (if (and (stringp (first optional-docstring-and-body))
                             (rest optional-docstring-and-body))
                        (first optional-docstring-and-body)
                        nil))
         (body (if docstring
                   (rest optional-docstring-and-body)
                   optional-docstring-and-body)))
    (when (member-if (lambda (arg)
                       (member arg '(&key &allow-other-keyss)))
                     args)
      (error "Only &aux, &optional and &rest arguments are allowed"))
    (let ((req-args 0) optional-args)
      (dolist (arg args)
        (case arg
          (&optional (setf optional-args 0))
          (&rest (setf optional-args t)
                 (return))
          (&aux (return))
          (t (if optional-args
                 (incf optional-args)
                 (incf req-args)))))
      (when (/= (1+ req-args) (length signature))
        (warn "Signature doesn't match the required arguments ~
                (plus return type)"))
      `(ensure-xrpc-method ',handler-tag ',name
                           ',(append signature optional-args)
                           ',docstring (lambda (,@args) ,@body)))))

;;; introspection per <URL:http://xmlrpc-c.sourceforge.net/introspection.html>:

(defun system-list-methods ()
  "Lists the methods that are defined on this server."
  ;; TODO: list non-system methods
  (values (append (mapcar #'car *system-methods*)
                  (loop for (tag . name) being the hash-keys
                        of *user-defined-methods*
                        when (equal tag *current-tag*)
                          collect name))
          :dwim-array))

(defun system-method-signature (method-name)
  "Returns an array of method signatures (arrays of strings listing first the 
return type, then the argument types)."
  ;; TODO: user-def methods.
  (values
   (mapcar #'xmlrpc-type-tag
           (lookup-method-signature *current-tag* method-name :fixup-p t))
   :dwim-array))

(defun system-method-help (method-name)
  "Returns a method's documentation as an XML-RPC string."
  (values  (or (documentation (lookup-method *current-tag* method-name)
                              'function)
               "undef")
           :string))

