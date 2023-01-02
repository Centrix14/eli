(defsystem "eli-examples"
  :description "Examples for ELI"
  :licence "GNU GPL v3"
  :depends-on (:asdf :eli)
  :components ((:file "examples/parser-class")
               (:file "examples/pars-char")
               (:file "examples/delimiterp")))
