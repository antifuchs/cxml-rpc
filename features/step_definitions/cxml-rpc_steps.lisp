(When* #?{^I call the method "([^"]*)" with (no arguments|arguments .*)$} (method-name arg-string)
  (setf (var :result)
        (xrpc:call "http://localhost:8080/RPC2" (format nil "test.~A" method-name)
                   (if (string= arg-string "no arguments")
                       '()
                       (read-from-string (subseq arg-string 10))))))

(Then* #?{^the return value should be the (\S+) (.*)$} (type val-string)
  (let ((val (read-from-string val-string)))
    (case (intern (string-upcase type) 'cl)
      (string
       (assert (string= (var :result) val)
               ()
               "Result ~S is not string= ~S" (var :result) val))
      (integer
       (assert (= (var :result) val))))))

(Given* #?{^the following method response:$} (string)
  (setf (var :method-response) string))

(When* #?{^I decode the method response$} ()
  (setf (var :decoded-method-response)
        (multiple-value-list
         (cxml-rpc::decode-response (var :method-response)))))

(flet ((check-value-equality (string)
         (assert (string= (first (var :decoded-method-response)) string)
                 ()
                 "~s isn't the expected value ~s" (first (var :decoded-method-response)) string)))
  
  (Then* #?{^the first value should be:$} (string)
   (check-value-equality string))

  (Then* #?{^the first value should be "([^"]*)"$} (string)
    (check-value-equality string)))
