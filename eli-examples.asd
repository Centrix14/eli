(defsystem "eli-examples"
  :description "Examples for ELI"
  :licence "GNU GPL v3"
  :depends-on (:asdf :eli)
  :components ((:file "examples/parser/read-next-word")
               (:file "examples/parser/separatorp")))
