(asdf:defsystem #:proper-name
  :author "Bruno Dias"
  :version "1.0.0"
  :serial t
  :depends-on (#:cl-unicode
               #:uax-15)
  :components ((:file "package")))
