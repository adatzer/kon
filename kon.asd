(defsystem "kon"
  :depends-on ("uiop"
               "array-operations"
               "yason"
               "hunchentoot")
  :components
  ((:file "kon-fun" :depends-on ())
   (:file "kon-params" :depends-on ("kon-fun"))
   (:file "kon-serv" :depends-on ("kon-params"))))
