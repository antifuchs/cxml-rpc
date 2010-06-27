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
