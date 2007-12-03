(in-package :cxml-rpc)


;; for debugging:
(defun encoded-request (function-name &rest args)
  (with-output-to-string (s)
    (funcall (apply #'encoder function-name args) s)))

;;; encoding

(defmacro do-tagged-sequence ((type value list) &body body)
  (let ((seq (gensym))
        (index (gensym)))
    `(let ((,seq ,list))
       (etypecase ,seq
         (list (loop for (,type ,value . nil) on ,list by #'cddr
                     do (progn ,@body)))
         (sequence
          (assert (zerop (rem (length ,seq) 2)))
          (loop for ,index from 0 below (length ,seq) by 2
                for ,type = (elt ,seq ,index)
                for ,value = (elt ,seq (1+ ,index))
                do (progn ,@body)))))))

(defun encode-param (type object)
  (with-element "param"
    (encode-value type object)))

(defun encode-value (type object)
  (with-element "value"
    (encode-object type object)))

(defun universal-time-to-xml-rpc-time-string (utime)
  (multiple-value-bind (second minute hour date month year day)
      (apply #'decode-universal-time utime
             (when *print-timestamps-in-utc-p* (list 0)))
    (declare (ignore day))
    (format nil "~d~2,'0d~2,'0d~A~2,'0d:~2,'0d:~2,'0d"
            year month date
            (if *print-timestamps-in-utc-p* #\Z #\T)
            hour minute second)))

(defun encode-time (utime)
  (make-instance 'xml-rpc-date
     :universal-time utime
     :iso8601 (universal-time-to-xml-rpc-time-string utime)))

(defun dwim-type-for (object)
  (etypecase object
    (boolean :boolean)
    ((signed-byte 32) :integer)
    (float :double)
    (symbol :string)
    (string :string)
    (integer :time) ; this feels wrong, but value is a utime with a
                    ; very high probability (:
    (xml-rpc-date :time)
    (file-stream :base64)
    ((vector (unsigned-byte 8)) :base64)
    (cons :dwim-struct)
    (sequence :dwim-array)))

(defgeneric encode-object (type object)
  (:method ((type (eql :boolean)) o)
    (with-element "boolean"
      (text
       (if o "1" "0"))))
  (:method ((type (eql :integer)) (o integer))
    (with-element (etypecase o
                    ((signed-byte 32) "i4"))
      (text (format nil "~D" o))))
  (:method ((type (eql :double)) (o number))
    (with-element "double"
      (text (format nil "~F" o))))
  (:method ((type (eql :string)) o)
    (with-element "string"
      (text (string o))))
  (:method ((type (eql :array)) (o sequence))
    (with-element "array"
      (with-element "data"
        (do-tagged-sequence (type value o)
          (encode-value type value)))))
  (:method ((type (eql :dwim-array)) (o sequence))
    (with-element "array"
      (with-element "data"
        (map nil (lambda (value)
                   (encode-value (dwim-type-for value) value))
             o))))
  (:method ((type (eql :base64)) (o file-stream))
    (let* ((length (file-length o))
           (vector (make-array length
                               :element-type (stream-element-type o))))
      (read-sequence vector o)
      (encode-object :base64 vector)))
  (:method ((type (eql :base64)) (o string))
    (with-element "base64"
      (text (cl-base64:string-to-base64-string o))))
  (:method ((type (eql :base64)) (o sequence))
    (with-element "base64"
      (text (cl-base64:usb8-array-to-base64-string o))))
  (:method ((type (eql :time)) (o integer))
    (with-element "dateTime.iso8601"
      (text (universal-time-to-xml-rpc-time-string o))))
  (:method ((type (eql :time)) (o xml-rpc-date))
    (with-element "dateTime.iso8601"
      (text (iso8601-of o))))
  (:method ((type (eql :struct)) (alist cons))
    (with-element "struct"
      (loop for (name value-type value) in alist
            do (with-element "member"
                 (with-element "name" (text name))
                 (encode-value value-type value)))))
  (:method ((type (eql :dwim-struct)) (alist cons))
    (with-element "struct"
      (loop for (name value) in alist
            do (with-element "member"
                 (with-element "name" (text name))
                 (encode-value (dwim-type-for value) value))))))