(defsystem "eli-tests"
  :description "ELI tests"
  :licence "GNU GPL v3"
  :depends-on (:asdf :eli)
  :components ((:file "t/parser/pars-char")
               (:file "t/parser/pars-stream")))
