(in-package :cxml-rpc)

(defun skip-characters (source)
  (apply #'concatenate 'string
         (loop while (eql :characters (klacks:peek source))
               collect (nth-value 1 (klacks:skip source :characters)))))

(defun skip* (source &rest args)
  (skip-characters source)
  (apply #'klacks:skip source args)
  (skip-characters source))

(defun invoke-expecting-element/consuming (source element continuation)
  (klacks:expecting-element (source element)
    (skip* source :start-element nil element)
    (multiple-value-prog1 (progn (funcall continuation source))
                          (skip-characters source))))

(defmacro expecting-element/consuming ((source lname) &body body)
  `(flet ((expecting-element-continuation (,source)
            ,@body))
     (invoke-expecting-element/consuming ,source ,lname
                                         #'expecting-element-continuation)))

(defun invoke-expecting-element/characters (source element continuation)
  (klacks:expecting-element (source element)
    (klacks:skip source :start-element nil element)
    (let ((characters (skip-characters source)))
      (funcall continuation source characters))))

(defmacro expecting-element/characters ((source lname character-var) &body body)
  `(flet ((expecting-element/characters-continuation
              (,source ,character-var)
            (declare (ignorable source))
            ,@body))
     (invoke-expecting-element/characters
      ,source ,lname
      #'expecting-element/characters-continuation)))

(defun decode-method-call (stream)
  (klacks:with-open-source (source (cxml:make-source stream))
    (klacks:find-element source "methodCall")
    (skip* source :start-element nil "methodCall")
    (let ((method-name (decode-method-name source)))
      (if (eql :end-element (klacks:peek source))
          method-name
          (expecting-element/consuming (source "params")
            (apply #'values
                   method-name
                   (loop while (eql :start-element (klacks:peek source))
                         for (value type) = (multiple-value-list
                                             (decode-parameter source))
                         collect value into params
                         collect type into param-types
                         do (skip-characters source)
                         finally (return (list param-types params)))))))))

(defun decode-response (stream)
  (klacks:with-open-source (source (cxml:make-source stream))
    (let (response-type)
      (klacks:find-element source "methodResponse")
      (klacks:consume source)
      (setf response-type (nth-value 2 (klacks:find-element source)))
      (when (equal response-type "fault")
        (expecting-element/consuming (source "fault")
          (let ((fault (decode-value source)))
            (error 'cxml-rpc-fault
                   :fault-code (third (assoc "faultCode" fault :test #'equal))
                   :fault-phrase (third (assoc "faultString" fault
                                               :test #'equal))))))
      (expecting-element/consuming (source "params")
        (decode-parameter source)))))

(defun decode-parameter (source)
  (expecting-element/consuming (source "param")
    (decode-value source)))

(defun decode-method-name (source)
  (multiple-value-prog1 (expecting-element/characters (source "methodName" name)
                          name)
                        (skip-characters source)))

(defun decode-name (source)
  (multiple-value-prog1 (expecting-element/characters (source "name" name)
                          name)
                        (skip-characters source)))

(defun decode-value (source)
  (klacks:expecting-element (source "value")
    (klacks:consume source)
    (multiple-value-bind (type val1 val2) (klacks:peek source)
      (declare (ignore val1))
      (ecase type
        (:characters ; Stupid: if no type is specified, it's a string...
         (multiple-value-prog1
           (multiple-value-bind (value type) (decode-object :lazy-string source)
             ;; ...but some impls insist on indenting the contents of
             ;; <values>:
             (if value
                 (values value type)
                 (multiple-value-prog1
                   (decode-object
                    (type-tag-for (nth-value 2 (klacks:peek source)))
                    source)
                   (skip-characters source))))))
        (:start-element
         (multiple-value-prog1 (decode-object (type-tag-for val2) source)
                               (skip-characters source)))
        (:end-element (values "" :string))))))

(defvar *xml-rpc-type-alist* '(("dateTime.iso8601" . :time)
                               ("string" . :string)
                               ("i4" . :integer)
                               ("int" . :integer)
                               ("double" . :double)
                               ("boolean" . :boolean)
                               ("base64" . :base64)
                               ("struct" . :struct)
                               ("array" . :array)))

(defun type-tag-for (tag)
  (cdr (assoc tag *xml-rpc-type-alist* :test #'equal)))

(defun xmlrpc-type-tag (lisp-tag)
  (car (find lisp-tag *xml-rpc-type-alist* :key 'cdr)))

(defun first-invalid-integer-position (string)
  (position-if-not (lambda (c) (or (eql c #\-) (eql c #\+) (digit-char-p c)))
                   string))

(defun decode-time (string)
  (let ((year (subseq string 0 4))
        (month (subseq string 4 6))
        (date (subseq string 6 8))
        (utc-marker (subseq string 8 9))
        (hour (subseq string 9 11))
        (minute (subseq string 12 14))
        (second (subseq string 15 17)))
    (apply #'encode-universal-time
           (mapcar #'parse-integer
                   `(,second ,minute ,hour ,date ,month ,year
                             ,@(when (equal utc-marker "Z")
                                 (list "0")))))))

(defgeneric decode-object (type source)
  (:method ((type (eql :lazy-string)) source)
    (let ((string (nth-value 1 (klacks:skip source :characters))))
      (when (eql :end-element (klacks:peek source))
        (values string :string))))
  (:method ((type (eql :string)) source)
    (expecting-element/characters (source "string" chars)
      (values chars :string)))
  (:method ((type (eql :time)) source)
    (expecting-element/characters (source "dateTime.iso8601" chars)
      (values (decode-time chars) :time)))
  (:method ((type (eql :integer)) source)
    (let ((integer-spec (nth-value 2 (klacks:peek source))))
      (expecting-element/characters (source integer-spec chars)
        (let ((value (parse-integer chars :junk-allowed t)))
          (when (first-invalid-integer-position chars)
            (error 'malformed-value-content
                   :type integer-spec :content chars))
          (values value :integer)))))
  (:method ((type (eql :boolean)) source)
    (expecting-element/characters (source "boolean" chars)
      (values (cond ((string= chars "1") t)
                    ((string= chars "0") nil)
                    (t (error 'malformed-value-content
                              :type "boolean" :content chars)))
              :boolean)))
  (:method ((type (eql :array)) source)
    (expecting-element/consuming (source "array")
      (expecting-element/consuming (source "data")
        (values
         (loop while (eql :start-element (klacks:peek source))
               for (value type) =  (multiple-value-list (decode-value source))
               collect type
               collect value
               do (skip-characters source))
         :array))))
  (:method ((type (eql :struct)) source)
    (expecting-element/consuming (source "struct")
      (values
       (loop while (eql :start-element (klacks:peek source))
             collect (expecting-element/consuming (source "member")
                       (let ((name (decode-name source)))
                         (multiple-value-bind (value type)
                             (decode-value source)
                           (list name type value))))
             do (skip-characters source))
       :struct)))
  (:method ((type (eql :base64)) source)
    (expecting-element/characters (source "base64" chars)
      (values (cl-base64:base64-string-to-usb8-array chars) :base64)))
  (:method ((type (eql :double)) source)
    (expecting-element/characters (source "double" chars)
      (when (find-if-not (lambda (c)
                           (or (digit-char-p c)
                               (member c '(#\. #\-))))
                         chars)
        (error 'malformed-value-content :type "double" :content chars))
      (handler-case (values (parse-number:parse-real-number chars) :double)
        (parse-error ()
          (error 'malformed-value-content :type "double" :content chars)))))
  (:method (type source)
    (error 'bad-type-specifier
           :element (nth-value 2 (klacks:peek source))
           :type-alist *xml-rpc-type-alist*)))