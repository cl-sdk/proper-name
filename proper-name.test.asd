(defsystem #:proper-name.test
  :author "Bruno Dias"
  :serial t
  :depends-on (#:fiveam
               #:proper-name)
  :components ((:file "test")))
